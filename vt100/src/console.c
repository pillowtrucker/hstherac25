/* The Therac-25 treatment console program (see console.h).
 *
 * Sources: N. G. Leveson and C. S. Turner, "An Investigation of the Therac-25 Accidents",
 * IEEE Computer 26(7), July 1993 - text in "double quotes" is quoted from it. Where the paper
 * does not say, the choice is marked ASSUMPTION and listed in vt100/README.md. */
#include "console.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- screen layout ----
 * "Figure A shows the screen layout." The figure is typeset in a proportional font, so the
 * columns below are scaled from it onto the VT100's 80 columns (ASSUMPTION); the labels are
 * padded so their colons line up, as in the monospaced version of the figure. The status block
 * is on the bottom three lines: "The command line at the lower right corner of the screen is the
 * cursor's normal position". */
#define ROW_NAME 1
#define ROW_MODE 2
#define ROW_HEADER 4
#define ROW_RATE 5
#define ROW_SITE 9 /* the six treatment-site rows */
#define ROW_DATE 21
#define ROW_CLOCK 22
#define ROW_OPR 23

#define COL_LABEL 2
#define COL_VALUE 18
#define COL_BEAM_LABEL 31
#define COL_BEAM 42
#define COL_ENERGY_LABEL 45
#define COL_RIGHT 69
#define COL_DOSE_LABEL 12
#define COL_ACTUAL_HEADER 31
#define COL_PRESCRIBED_HEADER 44
#define END_ACTUAL 37     /* ACTUAL values end just before this column */
#define END_PRESCRIBED 54 /* and PRESCRIBED ones before this one */
#define COL_STATUS_LABEL 25
#define COL_STATUS 33
#define STATUS_WIDTH 18
#define COL_OPMODE 52
#define COL_BEAM_WORD 58
#define COL_COMMAND 61

static const char *const site_labels[CONSOLE_SITE_ROWS] = {
    "GANTRY ROTATION (DEG)", "COLLIMATOR ROTATION (DEG)", "COLLIMATOR X (CM)",
    "COLLIMATOR Y (CM)",     "WEDGE NUMBER",              "ACCESSORY NUMBER"};

/* The room set-up in Figure A. ASSUMPTION: the patient is already set up when the page opens. */
static const double room_setup[CONSOLE_SITE_ROWS] = {0.0, 359.2, 14.2, 27.2, 1, 0};

typedef struct {
  int row, col, width;
  bool right; /* right-aligned in [col, col + width) */
} field_pos;

static field_pos pos_of(int f) {
  switch (f) {
    case F_NAME: return (field_pos){ROW_NAME, COL_VALUE, 20, false};
    case F_MODE: return (field_pos){ROW_MODE, COL_VALUE, 3, false};
    case F_BEAM: return (field_pos){ROW_MODE, COL_BEAM, 1, false};
    case F_ENERGY: return (field_pos){ROW_MODE, COL_RIGHT, 2, true};
    case F_COMMAND: return (field_pos){ROW_OPR, COL_COMMAND, 8, false};
    default:
      if (f <= F_TIME) return (field_pos){ROW_RATE + (f - F_RATE), END_PRESCRIBED - 6, 6, true};
      return (field_pos){ROW_SITE + (f - F_GANTRY), END_PRESCRIBED - 6, 6, true};
  }
}

static bool is_site(int f) { return f >= F_GANTRY && f <= F_ACCESSORY; }

/* ---- talking to the machine ---- */

static void machine(console *c, ExtCallType call, int beam, int collimator, int value) {
  wrap_external_call(c->machine, call, (BeamType)beam, (CollimatorPosition)collimator, value);
}

static void ask(console *c, StateInfoRequest r, char *out, size_t n) {
  char *s = request_state_info(c->machine, r);
  if (s) {
    snprintf(out, n, "%s", s);
    free_state_info(s);
  } else {
    out[0] = 0;
  }
}

static void bell(console *c) {
  static const uint8_t bel = 0x07;
  c->out(c->out_ctx, &bel, 1);
}

/* ---- field values ---- */

static bool parse_number(const char *s, double *v) {
  if (!*s) return false;
  char *end;
  *v = strtod(s, &end);
  return *end == 0;
}

static bool parse_int(const char *s, int *v) {
  if (!*s) return false;
  for (const char *p = s; *p; p++)
    if (*p < '0' || *p > '9') return false;
  *v = atoi(s);
  return true;
}

static void format_site(int i, double v, char *out, size_t n, bool prescribed) {
  if (i >= 4) snprintf(out, n, "%d", (int)lround(v));
  else if (prescribed && i < 2) snprintf(out, n, "%d", (int)floor(v)); /* "359.2" -> "359" */
  else snprintf(out, n, "%.1f", v);
}

/* "The system then compares the manually set values with those entered at the console. If
 * they match, a 'verified' message is displayed and treatment is permitted." Figure A verifies
 * 14.3 against 14.2 and 359 against 359.2, so there is some tolerance (ASSUMPTION: 1 degree,
 * 0.5 cm, exact for wedge and accessory numbers). */
static bool verified(const console *c, int i) {
  double v;
  if (!parse_number(c->committed[F_GANTRY + i], &v)) return false;
  double d = fabs(v - c->actual[i]);
  if (i < 2) {
    d = fmod(d, 360.0);
    if (d > 180.0) d = 360.0 - d;
    return d <= 1.0;
  }
  if (i < 4) return d <= 0.5 + 1e-9;
  return d < 1e-9;
}

static bool all_verified(const console *c) {
  for (int i = 0; i < CONSOLE_SITE_ROWS; i++)
    if (!verified(c, i)) return false;
  return true;
}

/* checks the field's text; normalises it and returns true if it can be accepted */
static bool valid(console *c, int f) {
  char *t = c->text[f];
  int n;
  double v;
  switch (f) {
    case F_NAME: return t[0] != 0;
    case F_MODE: return strcmp(t, "FIX") == 0; /* the only mode in Figure A */
    case F_BEAM: return strcmp(t, "X") == 0 || strcmp(t, "E") == 0;
    case F_ENERGY:
      if (!parse_int(t, &n)) return false;
      /* "the energy defaults to 25 MeV" in photon mode; electrons "from 5 to 25 MeV" */
      if (c->beam == 'X' ? n != 25 : (n < 5 || n > 25)) return false;
      snprintf(t, 24, "%d", n);
      return true;
    case F_RATE:
      if (!parse_int(t, &n) || n < 1 || n > 999) return false;
      snprintf(t, 24, "%d", n);
      return true;
    case F_MU:
      if (!parse_int(t, &n) || n < 1 || n > 9999) return false;
      snprintf(t, 24, "%d", n);
      return true;
    case F_TIME:
      if (!parse_number(t, &v) || v <= 0 || v >= 100) return false;
      snprintf(t, 24, "%.2f", v);
      return true;
    default:
      if (!is_site(f) || !parse_number(t, &v) || v < 0) return false;
      if (f - F_GANTRY < 2 && v >= 360) return false;
      if (f - F_GANTRY >= 4 && (v != floor(v) || v > 99)) return false;
      return true;
  }
}

/* "The keyboard handler parses the mode and energy level specified by the operator and places
 * an encoded result in another shared variable, the 2-byte mode/energy offset (MEOS) variable."
 * The turntable request is left to follow the mode. */
static void send_meos(console *c) {
  if (!c->beam || !c->energy) return;
  machine(c, ExtCallSendMEOS, c->beam == 'X' ? BeamTypeXRay : BeamTypeElectron,
          CollimatorPositionUndefined, c->energy * 1000);
}

static void commit(console *c, int f) {
  strcpy(c->committed[f], c->text[f]);
  if (f == F_BEAM) {
    c->beam = c->text[f][0];
    if (c->beam == 'X') {
      /* "except when the operator selects the photon mode, in which case the energy defaults
       * to 25 MeV" */
      strcpy(c->text[F_ENERGY], "25");
      strcpy(c->committed[F_ENERGY], "25");
      c->energy = 25;
    } else if (c->energy && (c->energy < 5 || c->energy > 25)) {
      c->energy = 0;
    }
    send_meos(c);
  } else if (f == F_ENERGY) {
    c->energy = atoi(c->text[f]);
    send_meos(c);
  } else if (f == F_MU) {
    machine(c, ExtCallPrescribeDose, 0, 0, atoi(c->text[f]));
  }
}

static void clear_prescription(console *c) {
  for (int f = F_BEAM; f <= F_COMMAND; f++) c->text[f][0] = c->committed[f][0] = 0;
  c->beam = 0;
  c->energy = 0;
  c->cmd[0] = 0;
  c->field = F_NAME;
  c->fresh = true;
}

/* ---- the command line ---- */

static bool phase_is(const console *c, const char *p) { return strcmp(c->phase, p) == 0; }

static void execute(console *c) {
  const char *cmd = c->text[F_COMMAND];
  if (!cmd[0]) return;
  if (strcmp(cmd, "B") == 0) {
    /* "She hit the one-key command 'B' (for 'beam on')" after "beam ready" */
    if (phase_is(c, "TP_SetupDone") && all_verified(c)) machine(c, ExtCallBeamOn, 0, 0, 0);
    else bell(c);
  } else if (strcmp(cmd, "P") == 0) {
    /* "the operator could press the 'P' key to 'proceed' and resume treatment" */
    if (phase_is(c, "TP_PauseTreatment")) machine(c, ExtCallProceed, 0, 0, 0);
    else bell(c);
  } else if (strcmp(cmd, "R") == 0) {
    /* "an 'R' reset command must be used and the whole prescription reentered" */
    machine(c, ExtCallReset, 0, 0, 0);
    clear_prescription(c);
    return;
  } else if (strcmp(cmd, "SET") == 0) {
    /* "presses the set button on the hand control or types 'set' at the console" */
    machine(c, ExtCallSet, 0, 0, 0);
  } else {
    bell(c);
  }
  c->text[F_COMMAND][0] = 0;
}

/* ---- keyboard handler ---- */

/* RETURN - and, the FDA noted, "the normal edit keys (down arrow, right arrow, or line feed)
 * will be interpreted as a CR". On the command line that means they execute what is typed. */
static void accept(console *c) {
  int f = c->field;
  if (f == F_COMMAND) {
    execute(c);
    return;
  }
  if (!c->text[f][0] && is_site(f)) {
    /* "operators could use a carriage return to merely copy the treatment site data" */
    int i = f - F_GANTRY;
    format_site(i, c->actual[i], c->text[f], sizeof c->text[f], true);
  }
  if (!valid(c, f)) {
    bell(c);
    c->fresh = false;
    return;
  }
  commit(c, f);
  c->field++;
  c->fresh = true;
  if (c->field == F_COMMAND) {
    c->text[F_COMMAND][0] = 0;
    /* "the data-entry completion variable only indicates that the cursor has been down to the
     * command line" */
    machine(c, ExtCallDataEntryComplete, 0, 0, 0);
  }
}

/* "the key used for moving the cursor back through the prescription sequence (i.e., cursor 'UP'
 * inscribed with an upward pointing arrow)". Anything typed in the field and not accepted with
 * RETURN is dropped (ASSUMPTION). */
static void cursor_up(console *c) {
  if (c->field == F_NAME) {
    bell(c);
    return;
  }
  if (c->field == F_COMMAND) {
    c->text[F_COMMAND][0] = 0;
    /* "Prescription editing is signified by cursor movement off the command line." */
    machine(c, ExtCallEditingTakingPlace, 0, 0, 0);
  } else {
    strcpy(c->text[c->field], c->committed[c->field]);
  }
  c->field--;
  c->fresh = true;
}

/* BACKSPACE, DELETE and left arrow: "One must use either the backspace or left arrow key to
 * edit." */
static void rub_out(console *c) {
  char *t = c->text[c->field];
  size_t n = strlen(t);
  c->fresh = false;
  if (n == 0) {
    bell(c);
    return;
  }
  t[n - 1] = 0;
}

static bool allowed(int f, char ch) {
  switch (f) {
    case F_NAME:
    case F_COMMAND: return ch >= ' ' && ch <= '~';
    case F_MODE: return ch >= 'A' && ch <= 'Z';
    case F_BEAM: return ch == 'X' || ch == 'E';
    case F_ENERGY:
    case F_RATE:
    case F_MU:
    case F_WEDGE:
    case F_ACCESSORY: return ch >= '0' && ch <= '9';
    default: return (ch >= '0' && ch <= '9') || ch == '.';
  }
}

static void type(console *c, char ch) {
  if (ch >= 'a' && ch <= 'z') ch = (char)(ch - 'a' + 'A');
  int f = c->field;
  if (!allowed(f, ch)) {
    bell(c);
    return;
  }
  char *t = c->text[f];
  if (c->fresh || pos_of(f).width == 1) t[0] = 0;
  c->fresh = false;
  size_t n = strlen(t);
  if ((int)n >= pos_of(f).width) {
    bell(c);
    return;
  }
  t[n] = ch;
  t[n + 1] = 0;
}

static void arrow(console *c, uint8_t final) {
  switch (final) {
    case 'A': cursor_up(c); break;
    case 'B':
    case 'C': accept(c); break;
    case 'D': rub_out(c); break;
    default: break;
  }
}

static void refresh(console *c);

void console_input(console *c, uint8_t b) {
  b &= 0x7F;
  /* cursor keys: ESC [ x (ANSI), ESC O x (cursor key mode), ESC x (VT52 mode) */
  if (c->esc_state == 1) {
    if (b == '[' || b == 'O') {
      c->esc_state = 2;
      return;
    }
    c->esc_state = 0;
    if (b >= 'A' && b <= 'D') arrow(c, b);
    refresh(c);
    return;
  }
  if (c->esc_state == 2) {
    if ((b >= '0' && b <= '9') || b == ';') return;
    c->esc_state = 0;
    arrow(c, b);
    refresh(c);
    return;
  }
  switch (b) {
    case 0x1B: c->esc_state = 1; return;
    case 0x0D:
    case 0x0A: accept(c); break;
    case 0x08:
    case 0x7F: rub_out(c); break;
    default:
      if (b >= ' ' && b <= '~') type(c, (char)b);
      else if (b == 0x09) bell(c);
      break;
  }
  refresh(c);
}

/* ---- treatment console screen processor ---- */

static void put(char w[24][80], int row, int col, const char *s) {
  for (; *s && col < 80; s++, col++)
    if (col >= 0) w[row][col] = *s;
}

static void put_right(char w[24][80], int row, int end, const char *s) {
  put(w, row, end - (int)strlen(s), s);
}

static void put_field(char w[24][80], int row, int col, int width, const char *s) {
  char buf[32];
  snprintf(buf, sizeof buf, "%.*s", width, s);
  put(w, row, col, buf);
}

/* What the TREAT line says for each Tphase. Only "TREAT PAUSE" is in the paper (Figure A); the
 * others are the Figure 2 subroutine names (ASSUMPTION). */
static const char *treat_word(const console *c) {
  if (phase_is(c, "TP_Reset")) return "RESET";
  if (phase_is(c, "TP_Datent")) return "DATA ENTRY";
  if (phase_is(c, "TP_SetupTest")) return "SET-UP TEST";
  if (phase_is(c, "TP_SetupDone")) return "SET-UP DONE";
  if (phase_is(c, "TP_PatientTreatment")) return "TREAT ON";
  if (phase_is(c, "TP_PauseTreatment")) return "TREAT PAUSE";
  if (phase_is(c, "TP_TerminateTreatment"))
    return strcmp(c->outcome, "TREATMENT OK") == 0 ? "TERMINATE" : "TREAT SUSPEND";
  return "";
}

static void civil_from_days(long long z, int *y, int *m, int *d) {
  z += 719468;
  long long era = (z >= 0 ? z : z - 146096) / 146097;
  long long doe = z - era * 146097;
  long long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  long long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  long long mp = (5 * doy + 2) / 153;
  *d = (int)(doy - (153 * mp + 2) / 5 + 1);
  *m = (int)(mp < 10 ? mp + 3 : mp - 9);
  *y = (int)(yoe + era * 400 + (*m <= 2));
}

static void build(console *c, char w[24][80]) {
  char buf[64];
  memset(w, ' ', 24 * 80);

  put(w, ROW_NAME, COL_LABEL, "PATIENT NAME  : ");
  put(w, ROW_NAME, COL_RIGHT, "A");
  put(w, ROW_NAME, COL_RIGHT + 6, "1");
  put(w, ROW_MODE, COL_LABEL, "TREATMENT MODE: ");
  put(w, ROW_MODE, COL_BEAM_LABEL, "BEAM TYPE: ");
  put(w, ROW_MODE, COL_ENERGY_LABEL, "ENERGY (KeV):");

  put(w, ROW_HEADER, COL_ACTUAL_HEADER, "ACTUAL");
  put(w, ROW_HEADER, COL_PRESCRIBED_HEADER, "PRESCRIBED");
  put(w, ROW_RATE, COL_DOSE_LABEL, "UNIT RATE/MINUTE");
  put(w, ROW_RATE + 1, COL_DOSE_LABEL, "MONITOR UNITS");
  put(w, ROW_RATE + 2, COL_DOSE_LABEL, "TIME (MIN)");

  /* the dose monitor: two channels; the beam never stays on long enough here to show a rate */
  put_right(w, ROW_RATE, END_ACTUAL, "0");
  snprintf(buf, sizeof buf, "%d %d", c->displayed_mu, c->displayed_mu);
  put_right(w, ROW_RATE + 1, END_ACTUAL, buf);
  int rate = atoi(c->committed[F_RATE]);
  snprintf(buf, sizeof buf, "%.2f", rate > 0 ? (double)c->displayed_mu / rate : 0.0);
  put_right(w, ROW_RATE + 2, END_ACTUAL, buf);

  for (int i = 0; i < CONSOLE_SITE_ROWS; i++) {
    put(w, ROW_SITE + i, COL_LABEL, site_labels[i]);
    format_site(i, c->actual[i], buf, sizeof buf, false);
    put_right(w, ROW_SITE + i, END_ACTUAL, buf);
    if (verified(c, i)) put(w, ROW_SITE + i, COL_RIGHT, "VERIFIED");
  }

  for (int f = 0; f < F_COUNT; f++) {
    field_pos p = pos_of(f);
    if (p.right) {
      snprintf(buf, sizeof buf, "%.*s", p.width, c->text[f]);
      put_right(w, p.row, p.col + p.width, buf);
    } else {
      put_field(w, p.row, p.col, p.width, c->text[f]);
    }
  }

  /* status block */
  static const char *const months[] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
  long long days = c->clock_s >= 0 ? c->clock_s / 86400 : (c->clock_s - 86399) / 86400;
  long long secs = c->clock_s - days * 86400;
  int y, m, d;
  civil_from_days(days, &y, &m, &d);
  put(w, ROW_DATE, COL_LABEL, "DATE  : ");
  snprintf(buf, sizeof buf, "%02d-%s-%02d", y % 100, months[m - 1], d);
  put(w, ROW_DATE, COL_LABEL + 8, buf);
  put(w, ROW_CLOCK, COL_LABEL, "TIME  : ");
  /* Figure A: "12:55. 8" */
  snprintf(buf, sizeof buf, "%2d:%02d.%2d", (int)(secs / 3600), (int)(secs / 60 % 60), (int)(secs % 60));
  put(w, ROW_CLOCK, COL_LABEL + 8, buf);
  put(w, ROW_OPR, COL_LABEL, "OPR ID: T25V02-R03");

  const char *system = "";
  if (c->set_prompt[0]) system = c->set_prompt; /* "Press set button" */
  else if (phase_is(c, "TP_SetupDone") && all_verified(c)) system = "BEAM READY";
  else if (phase_is(c, "TP_PatientTreatment")) system = "BEAM ON";
  put(w, ROW_DATE, COL_STATUS_LABEL, "SYSTEM: ");
  put_field(w, ROW_DATE, COL_STATUS, STATUS_WIDTH, system);
  put(w, ROW_CLOCK, COL_STATUS_LABEL, "TREAT : ");
  put_field(w, ROW_CLOCK, COL_STATUS, STATUS_WIDTH, treat_word(c));
  put(w, ROW_OPR, COL_STATUS_LABEL, "REASON: ");
  put_field(w, ROW_OPR, COL_STATUS, STATUS_WIDTH, c->reason);

  put(w, ROW_DATE, COL_OPMODE, "OP.MODE: TREAT");
  put(w, ROW_DATE, COL_RIGHT, "AUTO");
  put(w, ROW_CLOCK, COL_BEAM_WORD, c->beam == 'X' ? "X-RAY" : c->beam == 'E' ? "ELECTRON" : "");
  put(w, ROW_CLOCK, COL_RIGHT, "173777");
  put(w, ROW_OPR, COL_OPMODE, "COMMAND: ");
}

typedef struct {
  uint8_t buf[4096];
  size_t n;
} outbuf;

static void emit(outbuf *o, const char *s, size_t n) {
  if (o->n + n > sizeof o->buf) return;
  memcpy(o->buf + o->n, s, n);
  o->n += n;
}

static void move_cursor(console *c, outbuf *o, int row, int col) {
  if (c->cur_row == row && c->cur_col == col) return;
  char buf[16];
  int n;
  if (c->cur_row == row && c->cur_col == col + 1) n = snprintf(buf, sizeof buf, "\b");
  else n = snprintf(buf, sizeof buf, "\033[%d;%dH", row + 1, col + 1);
  emit(o, buf, (size_t)n);
  c->cur_row = row;
  c->cur_col = col;
}

/* Rewrites only what changed, then puts the cursor back where the operator is typing. */
static void refresh(console *c) {
  char w[24][80];
  outbuf o;
  o.n = 0;
  build(c, w);
  if (c->need_clear) {
    emit(&o, "\033[H\033[2J", 7);
    memset(c->shown, ' ', sizeof c->shown);
    c->cur_row = c->cur_col = 0;
    c->need_clear = false;
  }
  for (int r = 0; r < 24; r++) {
    int col = 0;
    while (col < 80) {
      if (w[r][col] == c->shown[r][col]) {
        col++;
        continue;
      }
      /* a run of changes, swallowing short unchanged gaps */
      int end = col + 1, last = col;
      while (end < 80 && end - last <= 4) {
        if (w[r][end] != c->shown[r][end]) last = end;
        end++;
      }
      move_cursor(c, &o, r, col);
      emit(&o, &w[r][col], (size_t)(last - col + 1));
      memcpy(&c->shown[r][col], &w[r][col], (size_t)(last - col + 1));
      c->cur_col = last + 1;
      col = last + 1;
    }
  }
  /* the cursor waits on the field it has just moved to (the next key replaces it), and follows
   * the text while the operator types */
  field_pos p = pos_of(c->field);
  int len = (int)strlen(c->text[c->field]);
  if (len > p.width) len = p.width;
  int start = p.right ? p.col + p.width - len : p.col;
  move_cursor(c, &o, p.row, c->fresh ? (p.right && len == 0 ? p.col + p.width - 1 : start) : start + len);
  if (o.n) c->out(c->out_ctx, o.buf, o.n);
}

static void poll_machine(console *c) {
  char buf[32];
  ask(c, RequestTreatmentOutcome, c->outcome, sizeof c->outcome);
  ask(c, RequestTreatmentState, c->phase, sizeof c->phase);
  ask(c, RequestReason, c->reason, sizeof c->reason);
  ask(c, RequestSetButtonPrompt, c->set_prompt, sizeof c->set_prompt);
  ask(c, RequestDisplayedDose, buf, sizeof buf);
  c->displayed_mu = atoi(buf);
}

void console_init(console *c, HsStablePtr m, console_out_fn out, void *ctx) {
  memset(c, 0, sizeof *c);
  c->machine = m;
  c->out = out;
  c->out_ctx = ctx;
  for (int i = 0; i < CONSOLE_SITE_ROWS; i++) c->actual[i] = room_setup[i];
  /* ASSUMPTION: the treatment mode is filled in, as in Figure A; the operator still types the
   * rest */
  strcpy(c->text[F_MODE], "FIX");
  strcpy(c->committed[F_MODE], "FIX");
  c->field = F_NAME;
  c->fresh = true;
  c->need_clear = true;
  c->cur_row = c->cur_col = -1;
  /* this console has a "B" command, so BEAM READY must wait for it */
  machine(c, ExtCallUseBeamOnKey, 0, 0, 0);
  poll_machine(c);
}

void console_power_cycle(console *c) {
  machine(c, ExtCallHardReset, 0, 0, 0);
  machine(c, ExtCallUseBeamOnKey, 0, 0, 0);
  c->text[F_NAME][0] = c->committed[F_NAME][0] = 0;
  clear_prescription(c);
  c->esc_state = 0;
  c->need_clear = true;
  poll_machine(c);
}

void console_tick(console *c, double now_ms, long long local_epoch_s) {
  c->clock_s = local_epoch_s;
  if (now_ms < c->next_pass) return;
  /* "Treatment console screen processor (run periodically)"; the scheduler's period is 0.1 s */
  c->next_pass = now_ms + 100.0;
  poll_machine(c);
  refresh(c);
}

void console_wanted_text(console *c, char *out) {
  char w[24][80];
  build(c, w);
  char *p = out;
  for (int r = 0; r < 24; r++) {
    memcpy(p, w[r], 80);
    p += 80;
    *p++ = '\n';
  }
  *p = 0;
}
