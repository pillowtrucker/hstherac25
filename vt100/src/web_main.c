/* Entry points for the browser (vt100/web/main.js). Everything runs on the page's one thread:
 * the page calls web_frame once per animation frame, and the simulator's own threads run in
 * between, woken by setTimeout (GHC's threadDelay on wasm). */
#include <stdio.h>
#include <string.h>

#include "session.h"

static session S;
static char text[VT_ROWS * (VT_COLS + 1) + 1];
static char info[512];

void web_init(double now_ms) { session_init(&S, 9600, now_ms); }

/* returns 1 if the raster was redrawn */
int web_frame(double now_ms, double local_epoch_s) {
  session_step(&S, now_ms, (long long)local_epoch_s);
  return vt_render(&S.term, now_ms) ? 1 : 0;
}

/* 800 x 240 bytes, intensity 0-3 */
const uint8_t *web_raster(void) { return &S.term.raster[0][0]; }

void web_key(int key, int down, double now_ms) { session_key(&S, key, down != 0, now_ms); }

void web_hand(int button) { session_hand(&S, button); }

void web_power(void) { session_power_cycle(&S); }

const char *web_text(void) {
  vt_text(&S.term, text);
  return text;
}

/* what the operator could not see, for the instructor panel: see RequestClass3 etc. */
const char *web_info(int request) {
  char *s = request_state_info(S.machine, (StateInfoRequest)request);
  snprintf(info, sizeof info, "%s", s ? s : "");
  if (s) free_state_info(s);
  return info;
}

int web_leds(void) { return S.term.leds; }

int web_take_clicks(void) {
  int n = S.term.clicks;
  S.term.clicks = 0;
  return n;
}

int web_take_bells(void) {
  int n = S.term.bells;
  S.term.bells = 0;
  return n;
}

/* SET-UP choice: 1 = blinking block, 0 = blinking underline */
void web_cursor_style(int block) {
  S.term.block_cursor = block != 0;
  S.term.dirty = true;
}
