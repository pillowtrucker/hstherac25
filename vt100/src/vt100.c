/* DEC VT100 terminal. See vt100.h for the sources. Quotes are from the VT100 Technical Manual
 * (EK-VT100-TM-003) and User Guide (EK-VT100-UG) as published on vt100.net. */
#include "vt100.h"

#include <stdio.h>
#include <string.h>

#include "vt100_font.inc"

enum { S_GROUND, S_ESC, S_ESC_INTER, S_CSI, S_CSI_IGNORE };

/* blink timing, from 60 Hz frames. "The blink rate is about half that of the cursor or about
 * 0.5 Hz": the cursor changes every 32 frames, blinking characters every 64. */
#define CURSOR_HALF_PERIOD_MS (32 * 1000.0 / 60.0)
#define BLINK_HALF_PERIOD_MS (64 * 1000.0 / 60.0)

/* "the timer decrements to zero. This takes about one-half a second. Then the key is sent to the
 * keyboard buffer a second time ... This time the count lasts about one thirtieth of a second." */
#define REPEAT_DELAY_MS 500.0
#define REPEAT_INTERVAL_MS (1000.0 / 30.0)

static void tx(vt100 *t, const char *s) {
  size_t n = strlen(s);
  if (t->tx_len + (int)n > (int)sizeof t->tx) return; /* keyboard buffer full: keys are lost */
  memcpy(t->tx + t->tx_len, s, n);
  t->tx_len += (int)n;
}

static void tx_byte(vt100 *t, uint8_t b) {
  if (t->tx_len < (int)sizeof t->tx) t->tx[t->tx_len++] = b;
}

static int last_col(const vt100 *t, int row) {
  return t->line_attr[row] == VT_LINE_NORMAL ? VT_COLS - 1 : VT_COLS / 2 - 1;
}

static void clear_cells(vt100 *t, int row, int from, int to) {
  for (int c = from; c <= to; c++) {
    t->cells[row][c].glyph = ' ';
    t->cells[row][c].attr = 0;
  }
  t->dirty = true;
}

static void reset_tabs(vt100 *t) {
  for (int c = 0; c < VT_COLS; c++) t->tabs[c] = c > 0 && c % 8 == 0;
}

static void full_reset(vt100 *t) {
  bool click = t->keyclick, block = t->block_cursor;
  uint8_t leds = t->leds & (1 << VT_LED_ONLINE);
  memset(t->cells, 0, sizeof t->cells);
  for (int r = 0; r < VT_ROWS; r++) clear_cells(t, r, 0, VT_COLS - 1);
  memset(t->line_attr, 0, sizeof t->line_attr);
  t->row = t->col = 0;
  t->wrap_pending = false;
  t->attr = 0;
  t->top = 0;
  t->bottom = VT_ROWS - 1;
  reset_tabs(t);
  t->g[0] = t->g[1] = 'B';
  t->gl = 0;
  t->lnm = t->decckm = t->decom = t->decscnm = t->deckpam = false;
  t->decawm = false; /* ASSUMPTION: SET-UP B "auto wrap" off */
  t->decarm = true;
  t->saved.row = t->saved.col = 0;
  t->saved.attr = 0;
  t->saved.origin_mode = false;
  t->saved.g[0] = t->saved.g[1] = 'B';
  t->saved.gl = 0;
  t->state = S_GROUND;
  t->keyclick = click;
  t->block_cursor = block;
  t->leds = leds;
  t->dirty = true;
}

void vt_init(vt100 *t) {
  memset(t, 0, sizeof *t);
  t->keyclick = true;
  t->block_cursor = true; /* ASSUMPTION: SET-UP "cursor" set to blinking block */
  t->leds = 1 << VT_LED_ONLINE;
  t->last_phase = -1;
  full_reset(t);
}

/* ---- screen operations ---- */

static void scroll_up(vt100 *t, int top, int bottom) {
  memmove(&t->cells[top], &t->cells[top + 1], sizeof t->cells[0] * (size_t)(bottom - top));
  memmove(&t->line_attr[top], &t->line_attr[top + 1], (size_t)(bottom - top));
  t->line_attr[bottom] = VT_LINE_NORMAL;
  clear_cells(t, bottom, 0, VT_COLS - 1);
}

static void scroll_down(vt100 *t, int top, int bottom) {
  memmove(&t->cells[top + 1], &t->cells[top], sizeof t->cells[0] * (size_t)(bottom - top));
  memmove(&t->line_attr[top + 1], &t->line_attr[top], (size_t)(bottom - top));
  t->line_attr[top] = VT_LINE_NORMAL;
  clear_cells(t, top, 0, VT_COLS - 1);
}

static void index_down(vt100 *t) { /* IND / LF */
  if (t->row == t->bottom)
    scroll_up(t, t->top, t->bottom);
  else if (t->row < VT_ROWS - 1)
    t->row++;
}

static void reverse_index(vt100 *t) { /* RI */
  if (t->row == t->top)
    scroll_down(t, t->top, t->bottom);
  else if (t->row > 0)
    t->row--;
}

static void move_to(vt100 *t, int row, int col) {
  int lo = t->decom ? t->top : 0, hi = t->decom ? t->bottom : VT_ROWS - 1;
  row += lo;
  if (row < lo) row = lo;
  if (row > hi) row = hi;
  t->row = row;
  if (col < 0) col = 0;
  if (col > last_col(t, row)) col = last_col(t, row);
  t->col = col;
  t->wrap_pending = false;
  t->dirty = true;
}

static uint8_t glyph_for(const vt100 *t, uint8_t c) {
  uint8_t set = t->g[t->gl];
  if (set == '0' || set == '2') { /* special graphics: 0x5F-0x7E live at ROM 0x00-0x1F */
    if (c >= 0x5F && c <= 0x7E) return (uint8_t)(c - 0x5F);
  } else if (set == 'A' && c == '#') {
    return 0x1E; /* pound sign */
  }
  return c;
}

static void put_char(vt100 *t, uint8_t c) {
  if (t->wrap_pending && t->decawm) {
    t->col = 0;
    index_down(t);
  }
  t->wrap_pending = false;
  int lc = last_col(t, t->row);
  if (t->col > lc) t->col = lc;
  t->cells[t->row][t->col].glyph = glyph_for(t, c);
  t->cells[t->row][t->col].attr = t->attr;
  if (t->col == lc)
    t->wrap_pending = true; /* the VT100 keeps the cursor on the last column */
  else
    t->col++;
  t->dirty = true;
}

static void erase_display(vt100 *t, int mode) {
  if (mode == 0) {
    clear_cells(t, t->row, t->col, VT_COLS - 1);
    for (int r = t->row + 1; r < VT_ROWS; r++) {
      clear_cells(t, r, 0, VT_COLS - 1);
      t->line_attr[r] = VT_LINE_NORMAL;
    }
  } else if (mode == 1) {
    for (int r = 0; r < t->row; r++) {
      clear_cells(t, r, 0, VT_COLS - 1);
      t->line_attr[r] = VT_LINE_NORMAL;
    }
    clear_cells(t, t->row, 0, t->col);
  } else if (mode == 2) {
    for (int r = 0; r < VT_ROWS; r++) {
      clear_cells(t, r, 0, VT_COLS - 1);
      t->line_attr[r] = VT_LINE_NORMAL;
    }
  }
}

static void erase_line(vt100 *t, int mode) {
  if (mode == 0) clear_cells(t, t->row, t->col, VT_COLS - 1);
  else if (mode == 1) clear_cells(t, t->row, 0, t->col);
  else if (mode == 2) clear_cells(t, t->row, 0, VT_COLS - 1);
}

static void save_cursor(vt100 *t) {
  t->saved.row = t->row;
  t->saved.col = t->col;
  t->saved.attr = t->attr;
  t->saved.origin_mode = t->decom;
  t->saved.g[0] = t->g[0];
  t->saved.g[1] = t->g[1];
  t->saved.gl = t->gl;
}

static void restore_cursor(vt100 *t) {
  t->row = t->saved.row;
  t->col = t->saved.col;
  t->attr = t->saved.attr;
  t->decom = t->saved.origin_mode;
  t->g[0] = t->saved.g[0];
  t->g[1] = t->saved.g[1];
  t->gl = t->saved.gl;
  t->wrap_pending = false;
  if (t->col > last_col(t, t->row)) t->col = last_col(t, t->row);
  t->dirty = true;
}

static int param(const vt100 *t, int i, int dflt) {
  return (i < t->nparams && t->params[i] > 0) ? t->params[i] : dflt;
}

static void set_mode(vt100 *t, bool on) {
  for (int i = 0; i < (t->nparams ? t->nparams : 1); i++) {
    int p = t->params[i];
    if (!t->private_marker) {
      if (p == 20) t->lnm = on;
      continue;
    }
    switch (p) {
      case 1: t->decckm = on; break;
      case 3: /* DECCOLM: this emulation only has 80 columns, but the side effects stay */
        erase_display(t, 2);
        t->top = 0;
        t->bottom = VT_ROWS - 1;
        move_to(t, 0, 0);
        break;
      case 5: t->decscnm = on; t->dirty = true; break;
      case 6: t->decom = on; move_to(t, 0, 0); break;
      case 7: t->decawm = on; break;
      case 8: t->decarm = on; break;
      default: break; /* 2 (VT52 mode), 4 (smooth scroll), 9 (interlace): not emulated */
    }
  }
}

static void sgr(vt100 *t) {
  for (int i = 0; i < (t->nparams ? t->nparams : 1); i++) {
    switch (t->params[i]) {
      case 0: t->attr = 0; break;
      case 1: t->attr |= VT_ATTR_BOLD; break;
      case 4: t->attr |= VT_ATTR_UNDERLINE; break;
      case 5: t->attr |= VT_ATTR_BLINK; break;
      case 7: t->attr |= VT_ATTR_REVERSE; break;
      default: break;
    }
  }
}

static void csi_dispatch(vt100 *t, uint8_t final) {
  char buf[32];
  switch (final) {
    case 'A': move_to(t, t->row - (t->decom ? t->top : 0) - param(t, 0, 1), t->col);
      if (!t->decom && t->row < t->top && t->row + param(t, 0, 1) >= t->top) t->row = t->top;
      break;
    case 'B': {
      int r = t->row + param(t, 0, 1);
      int lim = (t->row <= t->bottom) ? t->bottom : VT_ROWS - 1;
      t->row = r > lim ? lim : r;
      t->wrap_pending = false;
      t->dirty = true;
      break;
    }
    case 'C': t->col += param(t, 0, 1);
      if (t->col > last_col(t, t->row)) t->col = last_col(t, t->row);
      t->wrap_pending = false;
      t->dirty = true;
      break;
    case 'D': t->col -= param(t, 0, 1);
      if (t->col < 0) t->col = 0;
      t->wrap_pending = false;
      t->dirty = true;
      break;
    case 'H':
    case 'f': move_to(t, param(t, 0, 1) - 1, param(t, 1, 1) - 1); break;
    case 'J': erase_display(t, t->nparams ? t->params[0] : 0); break;
    case 'K': erase_line(t, t->nparams ? t->params[0] : 0); break;
    case 'm': sgr(t); break;
    case 'r': {
      int top = param(t, 0, 1) - 1, bottom = param(t, 1, VT_ROWS) - 1;
      if (bottom > VT_ROWS - 1) bottom = VT_ROWS - 1;
      if (top < bottom) {
        t->top = top;
        t->bottom = bottom;
        move_to(t, 0, 0);
      }
      break;
    }
    case 'h': set_mode(t, true); break;
    case 'l': set_mode(t, false); break;
    case 'g':
      if (param(t, 0, 0) == 0) t->tabs[t->col] = false;
      else if (t->params[0] == 3) memset(t->tabs, 0, sizeof t->tabs);
      break;
    case 'q': /* DECLL */
      for (int i = 0; i < (t->nparams ? t->nparams : 1); i++) {
        int p = t->params[i];
        if (p == 0) t->leds &= (uint8_t)~((1 << VT_LED_L1) | (1 << VT_LED_L2) | (1 << VT_LED_L3) | (1 << VT_LED_L4));
        else if (p >= 1 && p <= 4) t->leds |= (uint8_t)(1 << (VT_LED_L1 + p - 1));
      }
      break;
    case 'c': /* DA: a VT100 with the advanced video option */
      if (param(t, 0, 0) == 0) tx(t, "\033[?1;2c");
      break;
    case 'n':
      if (param(t, 0, 0) == 5) {
        tx(t, "\033[0n");
      } else if (param(t, 0, 0) == 6) {
        snprintf(buf, sizeof buf, "\033[%d;%dR", t->row - (t->decom ? t->top : 0) + 1, t->col + 1);
        tx(t, buf);
      }
      break;
    default: break;
  }
}

static void esc_dispatch(vt100 *t, uint8_t final) {
  if (t->intermediate == '#') {
    int la = -1;
    switch (final) {
      case '3': la = VT_LINE_DOUBLE_TOP; break;
      case '4': la = VT_LINE_DOUBLE_BOTTOM; break;
      case '5': la = VT_LINE_NORMAL; break;
      case '6': la = VT_LINE_DOUBLE_WIDTH; break;
      case '8': /* DECALN */
        for (int r = 0; r < VT_ROWS; r++) {
          t->line_attr[r] = VT_LINE_NORMAL;
          for (int c = 0; c < VT_COLS; c++) {
            t->cells[r][c].glyph = 'E';
            t->cells[r][c].attr = 0;
          }
        }
        t->dirty = true;
        break;
      default: break;
    }
    if (la >= 0) {
      t->line_attr[t->row] = (uint8_t)la;
      if (t->col > last_col(t, t->row)) t->col = last_col(t, t->row);
      t->dirty = true;
    }
    return;
  }
  if (t->intermediate == '(' || t->intermediate == ')') {
    t->g[t->intermediate == ')'] = final;
    return;
  }
  switch (final) {
    case '7': save_cursor(t); break;
    case '8': restore_cursor(t); break;
    case 'D': index_down(t); t->wrap_pending = false; t->dirty = true; break;
    case 'E': t->col = 0; index_down(t); t->wrap_pending = false; t->dirty = true; break;
    case 'M': reverse_index(t); t->wrap_pending = false; t->dirty = true; break;
    case 'H': t->tabs[t->col] = true; break;
    case 'c': full_reset(t); break;
    case '=': t->deckpam = true; break;
    case '>': t->deckpam = false; break;
    case 'Z': tx(t, "\033[?1;2c"); break;
    default: break;
  }
}

static void control(vt100 *t, uint8_t c) {
  switch (c) {
    case 0x07: t->bells++; break;
    case 0x08:
      if (t->col > 0) t->col--;
      t->wrap_pending = false;
      t->dirty = true;
      break;
    case 0x09: {
      int lc = last_col(t, t->row);
      do t->col++;
      while (t->col < lc && !t->tabs[t->col]);
      if (t->col > lc) t->col = lc;
      t->wrap_pending = false;
      t->dirty = true;
      break;
    }
    case 0x0A:
    case 0x0B:
    case 0x0C:
      index_down(t);
      if (t->lnm) t->col = 0;
      t->wrap_pending = false;
      t->dirty = true;
      break;
    case 0x0D: t->col = 0; t->wrap_pending = false; t->dirty = true; break;
    case 0x0E: t->gl = 1; break;
    case 0x0F: t->gl = 0; break;
    default: break; /* NUL, ENQ, XON/XOFF and the rest: nothing shown */
  }
}

void vt_receive(vt100 *t, uint8_t c) {
  c &= 0x7F;
  if (c == 0x18 || c == 0x1A) { /* CAN, SUB: abandon the sequence */
    t->state = S_GROUND;
    return;
  }
  if (c == 0x1B) {
    t->state = S_ESC;
    t->intermediate = 0;
    return;
  }
  if (c < 0x20) {
    control(t, c);
    return;
  }
  if (c == 0x7F) return;
  switch (t->state) {
    case S_GROUND: put_char(t, c); break;
    case S_ESC:
      if (c == '[') {
        t->state = S_CSI;
        t->nparams = 0;
        memset(t->params, 0, sizeof t->params);
        t->private_marker = false;
      } else if (c >= 0x20 && c <= 0x2F) {
        t->intermediate = c;
        t->state = S_ESC_INTER;
      } else {
        esc_dispatch(t, c);
        t->state = S_GROUND;
      }
      break;
    case S_ESC_INTER:
      if (c >= 0x20 && c <= 0x2F) break;
      esc_dispatch(t, c);
      t->state = S_GROUND;
      break;
    case S_CSI:
      if (c >= '0' && c <= '9') {
        if (t->nparams == 0) t->nparams = 1;
        int *p = &t->params[t->nparams - 1];
        if (*p < 10000) *p = *p * 10 + (c - '0');
      } else if (c == ';') {
        if (t->nparams == 0) t->nparams = 1;
        if (t->nparams < 16) t->params[t->nparams++] = 0;
      } else if (c == '?' && t->nparams == 0) {
        t->private_marker = true;
      } else if (c >= 0x40 && c <= 0x7E) {
        csi_dispatch(t, c);
        t->state = S_GROUND;
      } else {
        t->state = S_CSI_IGNORE;
      }
      break;
    case S_CSI_IGNORE:
      if (c >= 0x40 && c <= 0x7E) t->state = S_GROUND;
      break;
    default: t->state = S_GROUND; break;
  }
}

void vt_receive_buf(vt100 *t, const uint8_t *buf, size_t n) {
  for (size_t i = 0; i < n; i++) vt_receive(t, buf[i]);
}

/* ---- keyboard ---- */

static char shifted(char c) {
  static const char from[] = "1234567890-=`[];',./\\";
  static const char to[] = "!@#$%^&*()_+~{}:\"<>?|";
  if (c >= 'a' && c <= 'z') return (char)(c - 'a' + 'A');
  const char *p = strchr(from, c);
  return p ? to[p - from] : c;
}

/* sends the code for one press of the key; returns false for keys that send nothing */
static bool send_key(vt100 *t, int key) {
  if (key < 0x100) {
    char c = (char)key;
    if (t->shift) c = shifted(c);
    else if (t->caps && c >= 'a' && c <= 'z') c = (char)(c - 'a' + 'A');
    if (t->ctrl) {
      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) c = (char)(c & 0x1F);
      else if (c == ' ' || c == '@') c = 0x00;
      else if (c == '[' || c == '{') c = 0x1B;
      else if (c == '\\' || c == '|') c = 0x1C;
      else if (c == ']' || c == '}') c = 0x1D;
      else if (c == '~' || c == '`') c = 0x1E;
      else if (c == '?' || c == '/') c = 0x1F;
    }
    tx_byte(t, (uint8_t)c);
    return true;
  }
  char buf[4] = {0};
  switch (key) {
    case VK_RETURN:
    case VK_KP_ENTER:
      if (key == VK_KP_ENTER && t->deckpam) tx(t, "\033OM");
      else tx(t, t->lnm ? "\r\n" : "\r");
      return true;
    case VK_LINEFEED: tx_byte(t, 0x0A); return true;
    case VK_BACKSPACE: tx_byte(t, 0x08); return true;
    case VK_DELETE: tx_byte(t, 0x7F); return true;
    case VK_TAB: tx_byte(t, 0x09); return true;
    case VK_ESC: tx_byte(t, 0x1B); return true;
    case VK_NOSCROLL:
      t->xoff_sent = !t->xoff_sent;
      tx_byte(t, t->xoff_sent ? 0x13 : 0x11);
      return true;
    case VK_UP:
    case VK_DOWN:
    case VK_RIGHT:
    case VK_LEFT:
      buf[0] = 033;
      buf[1] = t->decckm ? 'O' : '[';
      buf[2] = "ABCD"[key - VK_UP == 0 ? 0 : key == VK_DOWN ? 1 : key == VK_RIGHT ? 2 : 3];
      tx(t, buf);
      return true;
    case VK_PF1:
    case VK_PF2:
    case VK_PF3:
    case VK_PF4:
      buf[0] = 033;
      buf[1] = 'O';
      buf[2] = (char)('P' + (key - VK_PF1));
      tx(t, buf);
      return true;
    case VK_KP_MINUS:
    case VK_KP_COMMA:
    case VK_KP_PERIOD:
      if (t->deckpam) {
        buf[0] = 033;
        buf[1] = 'O';
        buf[2] = key == VK_KP_MINUS ? 'm' : key == VK_KP_COMMA ? 'l' : 'n';
        tx(t, buf);
      } else {
        tx_byte(t, key == VK_KP_MINUS ? '-' : key == VK_KP_COMMA ? ',' : '.');
      }
      return true;
    case VK_BREAK: return true; /* a break condition on the line, not a character */
    default:
      if (key >= VK_KP0 && key <= VK_KP9) {
        if (t->deckpam) {
          buf[0] = 033;
          buf[1] = 'O';
          buf[2] = (char)('p' + (key - VK_KP0));
          tx(t, buf);
        } else {
          tx_byte(t, (uint8_t)('0' + (key - VK_KP0)));
        }
        return true;
      }
      return false; /* SET-UP (not emulated) */
  }
}

/* "a nonrepeat table contains a few keys that do not repeat. These are SET-UP, NO SCROLL, ESCAPE,
 * RETURN, BREAK, and ENTER." "A key with control cannot repeat" */
static bool repeats(const vt100 *t, int key) {
  if (t->ctrl || !t->decarm) return false;
  switch (key) {
    case VK_SETUP:
    case VK_NOSCROLL:
    case VK_ESC:
    case VK_RETURN:
    case VK_BREAK:
    case VK_KP_ENTER: return false;
    default: return true;
  }
}

void vt_key(vt100 *t, int key, bool down, double now_ms) {
  if (key == VK_SHIFT) {
    t->shift = down;
    return;
  }
  if (key == VK_CTRL) {
    t->ctrl = down;
    return;
  }
  if (key == VK_CAPSLOCK) {
    if (down) t->caps = !t->caps;
    return;
  }
  if (!down) {
    if (key == t->held_key) t->held_key = 0;
    return;
  }
  /* "SHIFT or CTRL keys do not generate any keyclick" - every code-sending key clicks */
  if (send_key(t, key) && t->keyclick) t->clicks++;
  if (repeats(t, key)) {
    t->held_key = key;
    t->repeat_at = now_ms + REPEAT_DELAY_MS;
  } else {
    t->held_key = 0;
  }
}

void vt_tick(vt100 *t, double now_ms) {
  int guard = 0;
  while (t->held_key && now_ms >= t->repeat_at && guard++ < 8) {
    if (send_key(t, t->held_key) && t->keyclick) t->clicks++;
    t->repeat_at += REPEAT_INTERVAL_MS;
  }
  if (t->held_key && now_ms >= t->repeat_at) t->repeat_at = now_ms + REPEAT_INTERVAL_MS;
}

int vt_take_tx(vt100 *t, uint8_t *out, int max) {
  int n = t->tx_len < max ? t->tx_len : max;
  memcpy(out, t->tx, (size_t)n);
  memmove(t->tx, t->tx + n, (size_t)(t->tx_len - n));
  t->tx_len -= n;
  return n;
}

/* ---- video ---- */

/* the ROM row shown on each of the ten scans of a character row: the first scan shows row 15
 * (only line-drawing characters use it, so vertical lines join up), the rest rows 0-8 */
static int rom_row(int scan) { return scan == 0 ? 15 : scan - 1; }

/* the 10 dots of one scan of a glyph, before the dot stretcher. "if the final bit in any row of
 * character data is set, the VT100 will 'replicate' that bit across the rest of cell" */
static unsigned glyph_dots(uint8_t glyph, int scan) {
  unsigned bits = vt100_rom[glyph & 0x7F][rom_row(scan)];
  unsigned dots = bits << 2; /* dots 0-7 in bits 9..2 */
  if (bits & 1) dots |= 3;
  return dots; /* bit 9 = leftmost dot */
}

bool vt_render(vt100 *t, double now_ms) {
  int cursor_on = ((long)(now_ms / CURSOR_HALF_PERIOD_MS) & 1) == 0;
  int blink_on = ((long)(now_ms / BLINK_HALF_PERIOD_MS) & 1) == 0;
  int phase = cursor_on | blink_on << 1;
  t->raster_changed = false;
  if (!t->dirty && phase == t->last_phase) return false;
  t->dirty = false;
  t->last_phase = phase;
  t->raster_changed = true;

  uint8_t on[VT_DOTS + 1];
  uint8_t cellx[VT_DOTS];
  for (int r = 0; r < VT_ROWS; r++) {
    int la = t->line_attr[r];
    int wide = la != VT_LINE_NORMAL;
    int ncols = wide ? VT_COLS / 2 : VT_COLS;
    for (int s = 0; s < VT_CELL_SCANS; s++) {
      int gs = la == VT_LINE_DOUBLE_TOP ? s / 2 : la == VT_LINE_DOUBLE_BOTTOM ? 5 + s / 2 : s;
      /* character video into the shift register */
      memset(on, 0, sizeof on);
      for (int c = 0; c < ncols; c++) {
        unsigned dots = glyph_dots(t->cells[r][c].glyph, gs);
        for (int i = 0; i < VT_CELL_DOTS; i++) {
          int bit = (dots >> (9 - i)) & 1;
          if (wide) {
            on[c * 20 + 2 * i + 1] = on[c * 20 + 2 * i + 2] = (uint8_t)bit;
            cellx[c * 20 + 2 * i] = cellx[c * 20 + 2 * i + 1] = (uint8_t)c;
          } else {
            on[c * 10 + i + 1] = (uint8_t)bit;
            cellx[c * 10 + i] = (uint8_t)c;
          }
        }
      }
      /* "The dot stretcher works by delaying the VIDEO IN H signal by one dot time ... and then
       * ORing the undelayed and delayed signals." on[x + 1] holds dot x, on[x] dot x - 1. */
      uint8_t *out = t->raster[r * VT_CELL_SCANS + s];
      for (int x = 0; x < VT_DOTS; x++) {
        int lit = on[x + 1] | on[x];
        int c = cellx[x];
        const vt_cell *cell = &t->cells[r][c];
        int attr = cell->attr;
        int is_cursor = r == t->row && c == (t->col > ncols - 1 ? ncols - 1 : t->col);
        int rev = ((attr & VT_ATTR_REVERSE) != 0) ^ t->decscnm;
        if ((attr & VT_ATTR_BLINK) && rev && !blink_on) rev ^= 1; /* blinking reverse: flips */
        if (is_cursor && t->block_cursor && cursor_on) rev ^= 1;
        int underline = gs == 8 && ((attr & VT_ATTR_UNDERLINE) || (is_cursor && !t->block_cursor && cursor_on));
        /* "Underline causes the ninth scan of a character to be forced to white of the same
         * intensity as the character for nonreversed characters, and to black for reverse" */
        int pix = underline ? !rev : (rev ? !lit : lit);
        int level;
        if (rev) {
          /* reverse characters have a dim background; bold and reverse give normal */
          level = (attr & VT_ATTR_BOLD) ? 2 : 1;
          if (is_cursor && !(attr & VT_ATTR_REVERSE) && !t->decscnm) level = 2;
        } else {
          level = (attr & VT_ATTR_BOLD) ? 3 : 2;
          /* "Blink applied to nonreverse characters causes them to alternate between their usual
           * intensity and the next lower intensity" */
          if ((attr & VT_ATTR_BLINK) && !blink_on) level--;
        }
        out[x] = pix ? (uint8_t)level : 0;
      }
    }
  }
  return true;
}

void vt_text(const vt100 *t, char *out) {
  char *p = out;
  for (int r = 0; r < VT_ROWS; r++) {
    int ncols = t->line_attr[r] == VT_LINE_NORMAL ? VT_COLS : VT_COLS / 2;
    for (int c = 0; c < ncols; c++) {
      uint8_t g = t->cells[r][c].glyph;
      *p++ = g >= 0x20 && g < 0x7F ? (char)g : (char)(g + 0x5F); /* graphics back to their codes */
    }
    *p++ = '\n';
  }
  *p = 0;
}
