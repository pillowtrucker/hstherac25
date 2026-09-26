/* A DEC VT100, emulated from DEC's own documentation:
 *   VT100 User Guide (EK-VT100-UG) - codes the keyboard sends, control sequences
 *   VT100 Technical Manual (EK-VT100-TM), chapter 4 - raster, dot stretcher, attributes,
 *   blink rates, bell, keyclick, auto repeat
 * Basic VT100 without the Advanced Video Option is what matters here, but the AVO attributes
 * (bold, blink, underline, reverse on any character) are implemented too. 80 columns only. */
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define VT_COLS 80
#define VT_ROWS 24
#define VT_CELL_DOTS 10  /* "each character ... is made up of a matrix of dots, ten wide and ten high" */
#define VT_CELL_SCANS 10
#define VT_DOTS (VT_COLS * VT_CELL_DOTS)    /* "There are 800 dots in a scan" */
#define VT_SCANS (VT_ROWS * VT_CELL_SCANS)  /* "and the raster is made of 240 scans" */

/* Keys, by position on the VT100 keyboard. The typewriter keys are named by the character they
 * send unshifted ('a', '1', '[', ' ' ...); everything else has a code here. */
enum vt_key {
  VK_RETURN = 0x100,
  VK_LINEFEED,
  VK_BACKSPACE,
  VK_DELETE,
  VK_TAB,
  VK_ESC,
  VK_BREAK,
  VK_NOSCROLL,
  VK_SETUP,
  VK_UP,
  VK_DOWN,
  VK_LEFT,
  VK_RIGHT,
  VK_PF1,
  VK_PF2,
  VK_PF3,
  VK_PF4,
  VK_KP0, /* VK_KP0 + n for digit n */
  VK_KP9 = VK_KP0 + 9,
  VK_KP_MINUS,
  VK_KP_COMMA,
  VK_KP_PERIOD,
  VK_KP_ENTER,
  VK_SHIFT,
  VK_CTRL,
  VK_CAPSLOCK
};

/* character attributes */
#define VT_ATTR_BOLD 1
#define VT_ATTR_UNDERLINE 2
#define VT_ATTR_BLINK 4
#define VT_ATTR_REVERSE 8

/* line attributes */
enum { VT_LINE_NORMAL, VT_LINE_DOUBLE_WIDTH, VT_LINE_DOUBLE_TOP, VT_LINE_DOUBLE_BOTTOM };

/* LEDs, bit numbers in vt100.leds */
enum { VT_LED_ONLINE, VT_LED_LOCAL, VT_LED_KBDLOCKED, VT_LED_L1, VT_LED_L2, VT_LED_L3, VT_LED_L4 };

typedef struct {
  uint8_t glyph; /* index into the character ROM (graphics characters are 0x01-0x1F there) */
  uint8_t attr;
} vt_cell;

typedef struct {
  int row, col;
  uint8_t attr;
  bool origin_mode;
  uint8_t g[2];
  int gl;
} vt_saved_cursor;

typedef struct vt100 {
  vt_cell cells[VT_ROWS][VT_COLS];
  uint8_t line_attr[VT_ROWS];
  int row, col;
  bool wrap_pending; /* the VT100 stays on the last column until the next printable character */
  uint8_t attr;
  int top, bottom; /* scrolling region, inclusive, 0-based */
  bool tabs[VT_COLS];
  uint8_t g[2]; /* designated character sets: 'B' ASCII, 'A' UK, '0' special graphics */
  int gl;       /* 0 = G0 (SI), 1 = G1 (SO) */
  vt_saved_cursor saved;

  /* modes */
  bool lnm, decckm, decawm, decom, decscnm, deckpam, decarm;
  bool block_cursor; /* SET-UP: blinking block (true) or blinking underline */
  bool keyclick;

  /* parser */
  int state;
  int params[16];
  int nparams;
  bool private_marker; /* '?' */
  uint8_t intermediate;

  /* keyboard */
  bool shift, ctrl, caps;
  int held_key;          /* the key being auto-repeated, 0 if none */
  int keys_down;
  double repeat_at;      /* when the held key is sent again */
  bool xoff_sent;        /* NO SCROLL */

  /* to the host (serial line), filled by the keyboard and by answers to DA/DSR */
  uint8_t tx[256];
  int tx_len;

  /* sounds for the host page to play, counted since the last vt_take_sounds */
  int clicks, bells;

  uint8_t leds;

  /* 800 x 240 dots, each 0 (black), 1 (dim), 2 (normal), 3 (bright) - the four levels the
   * DC012 video processor can put out */
  uint8_t raster[VT_SCANS][VT_DOTS];
  bool dirty;         /* cells changed since the last render */
  int last_phase;     /* blink/cursor phase of the last render */
  bool raster_changed; /* set by vt_render when the raster was redrawn */
} vt100;

void vt_init(vt100 *t);
/* bytes from the host */
void vt_receive(vt100 *t, uint8_t byte);
void vt_receive_buf(vt100 *t, const uint8_t *buf, size_t n);
/* keyboard; now_ms drives auto repeat */
void vt_key(vt100 *t, int key, bool down, double now_ms);
void vt_tick(vt100 *t, double now_ms);
/* bytes to send to the host; returns how many were copied (and removes them) */
int vt_take_tx(vt100 *t, uint8_t *out, int max);
/* redraws the raster if anything visible changed; returns true if it did */
bool vt_render(vt100 *t, double now_ms);
/* the 24 lines as text (for tests and the screen-reader mirror); out must hold 24*81 bytes */
void vt_text(const vt100 *t, char *out);
