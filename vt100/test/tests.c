/* Tests for the VT100 console: the terminal on its own, then the whole installation driven
 * by VT100 keystrokes against the real simulator. The scenarios run in parallel (about 40 s). */
#define _POSIX_C_SOURCE 200809L
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "session.h"

typedef struct {
  const char *name;
  char failure[4096];
} result;

static void fail(result *r, const char *fmt, ...) {
  if (r->failure[0]) return;
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(r->failure, sizeof r->failure, fmt, ap);
  va_end(ap);
}

#define CHECK(r, cond, ...)        \
  do {                             \
    if (!(cond)) fail(r, __VA_ARGS__); \
  } while (0)

/* ---- the terminal on its own ---- */

static void feed(vt100 *t, const char *s) { vt_receive_buf(t, (const uint8_t *)s, strlen(s)); }

static const char *line(const vt100 *t, int row, char *buf) {
  char all[VT_ROWS * (VT_COLS + 1) + 1];
  vt_text(t, all);
  memcpy(buf, all + row * (VT_COLS + 1), VT_COLS);
  buf[VT_COLS] = 0;
  return buf;
}

static void terminal_tests(result *r) {
  static vt100 t; /* big */
  char buf[VT_COLS + 1];
  uint8_t tx[64];
  int n;

  vt_init(&t);
  feed(&t, "\033[3;5Hhello");
  CHECK(r, strncmp(line(&t, 2, buf) + 4, "hello", 5) == 0, "CUP: got [%s]", buf);
  CHECK(r, t.row == 2 && t.col == 9, "cursor after text at %d,%d", t.row, t.col);
  feed(&t, "\033[1;7H\033[K");
  CHECK(r, strncmp(line(&t, 2, buf) + 4, "hello", 5) == 0, "EL on another line");
  feed(&t, "\033[3;7H\033[K");
  CHECK(r, strncmp(line(&t, 2, buf) + 4, "he   ", 5) == 0, "EL 0: got [%s]", buf);
  feed(&t, "\033[2J");
  CHECK(r, strspn(line(&t, 2, buf), " ") == VT_COLS, "ED 2 left [%s]", buf);

  /* the VT100 stays on the last column and wraps with the next character, if auto wrap is on */
  feed(&t, "\033[?7h\033[1;80Hab");
  CHECK(r, line(&t, 0, buf)[79] == 'a' && line(&t, 1, buf)[0] == 'b', "auto wrap");
  feed(&t, "\033[?7l\033[5;80Hab");
  CHECK(r, line(&t, 4, buf)[79] == 'b' && line(&t, 5, buf)[0] == ' ', "no auto wrap: overwrite");

  /* scrolling region and index */
  feed(&t, "\033[2J\033[1;1Htop\033[24;1Hbottom\n");
  CHECK(r, strncmp(line(&t, 22, buf), "bottom", 6) == 0, "LF at the bottom scrolls");
  CHECK(r, line(&t, 0, buf)[0] == ' ', "top line scrolled away");

  /* special graphics: 'q' is the horizontal line (ROM 0x12) */
  feed(&t, "\033[2J\033[H\033(0q\033(Bq");
  CHECK(r, t.cells[0][0].glyph == 0x12 && t.cells[0][1].glyph == 'q', "special graphics");

  /* answers */
  feed(&t, "\033[5;10H\033[6n");
  n = vt_take_tx(&t, tx, sizeof tx);
  CHECK(r, n == 7 && memcmp(tx, "\033[5;10R", 7) == 0, "CPR answer");
  feed(&t, "\033[c");
  n = vt_take_tx(&t, tx, sizeof tx);
  CHECK(r, n == 7 && memcmp(tx, "\033[?1;2c", 7) == 0, "DA answer");

  /* raster: '|' is ROM 0x10 on rows 0-2: dot 3, stretched into dot 4, from scan 1 */
  vt_init(&t);
  t.block_cursor = false;
  feed(&t, "|\033[5;1H");
  vt_render(&t, 1000.0);
  CHECK(r, t.raster[0][3] == 0, "scan 0 shows ROM row 15 (blank for '|')");
  CHECK(r, t.raster[1][2] == 0 && t.raster[1][3] == 2 && t.raster[1][4] == 2 && t.raster[1][5] == 0,
        "dot stretcher: got %d %d %d %d", t.raster[1][2], t.raster[1][3], t.raster[1][4], t.raster[1][5]);
  /* a horizontal line character fills its whole cell width (last dot replicated) and joins up */
  feed(&t, "\033[1;3H\033(0qq\033(B");
  vt_render(&t, 1000.0);
  int y = -1;
  for (int s = 0; s < 10; s++)
    if (t.raster[s][25]) y = s;
  CHECK(r, y >= 0, "graphics line not drawn");
  if (y >= 0)
    for (int x = 20; x < 40; x++) CHECK(r, t.raster[y][x] == 2, "line gap at dot %d", x);
  /* underline: the ninth scan */
  feed(&t, "\033[2;1H\033[4m \033[m");
  vt_render(&t, 1000.0);
  CHECK(r, t.raster[10 + 8][0] == 2 && t.raster[10 + 8][9] == 2 && t.raster[10 + 7][0] == 0, "underline scan");
  /* bold is the bright level */
  feed(&t, "\033[3;1H\033[1m|\033[m");
  vt_render(&t, 1000.0);
  CHECK(r, t.raster[21][3] == 3, "bold intensity %d", t.raster[21][3]);

  /* keyboard */
  vt_init(&t);
  vt_key(&t, VK_UP, true, 0);
  vt_key(&t, VK_UP, false, 1);
  n = vt_take_tx(&t, tx, sizeof tx);
  CHECK(r, n == 3 && memcmp(tx, "\033[A", 3) == 0, "up arrow");
  feed(&t, "\033[?1h");
  vt_key(&t, VK_UP, true, 0);
  vt_key(&t, VK_UP, false, 1);
  n = vt_take_tx(&t, tx, sizeof tx);
  CHECK(r, n == 3 && memcmp(tx, "\033OA", 3) == 0, "up arrow in cursor key mode");
  vt_key(&t, VK_BACKSPACE, true, 0);
  vt_key(&t, VK_DELETE, true, 0);
  vt_key(&t, VK_LINEFEED, true, 0);
  vt_key(&t, VK_SHIFT, true, 0);
  vt_key(&t, 'x', true, 0);
  vt_key(&t, VK_SHIFT, false, 0);
  vt_key(&t, VK_CTRL, true, 0);
  vt_key(&t, 'c', true, 0);
  vt_key(&t, VK_CTRL, false, 0);
  n = vt_take_tx(&t, tx, sizeof tx);
  CHECK(r, n == 5 && memcmp(tx, "\b\x7f\nX\x03", 5) == 0, "BS DEL LF shift ctrl");
  CHECK(r, t.clicks == 7, "keyclicks: %d", t.clicks);
  /* auto repeat: half a second, then 30 a second; RETURN never repeats */
  vt_init(&t);
  vt_key(&t, 'a', true, 0);
  vt_tick(&t, 499);
  CHECK(r, t.tx_len == 1, "repeat too early");
  vt_tick(&t, 500);
  CHECK(r, t.tx_len == 2, "no repeat after 0.5 s");
  for (double ms = 500; ms <= 1500; ms += 16.7) vt_tick(&t, ms); /* called once a frame */
  CHECK(r, t.tx_len >= 31 && t.tx_len <= 33, "repeat rate: %d in 1.5 s", t.tx_len);
  vt_key(&t, 'a', false, 1500);
  vt_take_tx(&t, tx, sizeof tx);
  vt_take_tx(&t, tx, sizeof tx);
  vt_key(&t, VK_RETURN, true, 2000);
  vt_tick(&t, 4000);
  CHECK(r, t.tx_len == 1, "RETURN repeated");
  feed(&t, "\a");
  CHECK(r, t.bells == 1, "bell");
}

/* ---- the whole installation ---- */

static double now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

static void run_for(session *s, double ms) {
  double end = now_ms() + ms;
  struct timespec pause = {0, 2 * 1000 * 1000};
  do {
    session_step(s, now_ms(), (long long)time(NULL));
    nanosleep(&pause, NULL);
  } while (now_ms() < end);
}

static void press(session *s, int key) {
  session_key(s, key, true, now_ms());
  run_for(s, 15);
  session_key(s, key, false, now_ms());
  run_for(s, 15);
}

/* types a string on the VT100 keyboard; \r is RETURN */
static void type(session *s, const char *str) {
  for (; *str; str++) {
    char ch = *str;
    if (ch == '\r') press(s, VK_RETURN);
    else if (ch >= 'A' && ch <= 'Z') {
      session_key(s, VK_SHIFT, true, now_ms());
      press(s, ch - 'A' + 'a');
      session_key(s, VK_SHIFT, false, now_ms());
    } else press(s, ch);
  }
}

/* the same at a person's pace: a key every gap_ms */
static void type_at(session *s, const char *str, double gap_ms) {
  for (; *str; str++) {
    char one[2] = {*str, 0};
    type(s, one);
    run_for(s, gap_ms - 30);
  }
}

static void screen(session *s, char *out) { vt_text(&s->term, out); }

static bool on_screen(session *s, const char *what) {
  static __thread char buf[VT_ROWS * (VT_COLS + 1) + 1];
  screen(s, buf);
  return strstr(buf, what) != NULL;
}

/* the text after a label on the screen, up to two spaces */
static void field(session *s, const char *label, char *out, size_t n) {
  char buf[VT_ROWS * (VT_COLS + 1) + 1];
  screen(s, buf);
  const char *p = strstr(buf, label);
  out[0] = 0;
  if (!p) return;
  p += strlen(label);
  while (*p == ' ') p++;
  size_t i = 0;
  while (p[i] && p[i] != '\n' && !(p[i] == ' ' && p[i + 1] == ' ') && i + 1 < n) {
    out[i] = p[i];
    i++;
  }
  out[i] = 0;
}

static bool wait_for(session *s, const char *what, double timeout_ms) {
  double end = now_ms() + timeout_ms;
  while (now_ms() < end) {
    if (on_screen(s, what)) return true;
    run_for(s, 20);
  }
  return on_screen(s, what);
}

static int sim_int(session *s, StateInfoRequest q) {
  char *p = request_state_info(s->machine, q);
  int v = atoi(p);
  free_state_info(p);
  return v;
}

static void dump(session *s, result *r) {
  char buf[VT_ROWS * (VT_COLS + 1) + 1];
  screen(s, buf);
  size_t n = strlen(r->failure);
  snprintf(r->failure + n, sizeof r->failure - n, "\n--- screen ---\n%s", buf);
}

/* fills in the whole prescription from the patient name down to the command line */
static void enter_prescription(session *s, char beam) {
  type(s, "TEST\r\r"); /* name, treatment mode FIX */
  char b[3] = {beam, '\r', 0};
  type(s, b);
  type(s, "\r");     /* energy: 25 */
  type(s, "200\r");  /* unit rate/minute */
  type(s, "202\r");  /* monitor units */
  type(s, "1\r");    /* time (min) */
  type(s, "\r\r\r\r\r\r"); /* "a carriage return to merely copy the treatment site data" */
}

/* press P through the machine's nuisance pauses until the treatment ends */
static bool finish(session *s) {
  for (int i = 0; i < 10; i++) {
    double end = now_ms() + 3000;
    while (now_ms() < end && !on_screen(s, "TREAT PAUSE") && !on_screen(s, "TERMINATE") &&
           !on_screen(s, "TREAT SUSPEND"))
      run_for(s, 20);
    if (on_screen(s, "TERMINATE") || on_screen(s, "TREAT SUSPEND")) return true;
    if (!on_screen(s, "TREAT PAUSE")) return false;
    type(s, "P\r");
    run_for(s, 400);
  }
  return false;
}

static void paints_figure_a(result *r) {
  static session s;
  session_init(&s, 9600, now_ms());
  double start = now_ms();
  CHECK(r, wait_for(&s, "COMMAND:", 5000), "screen never painted");
  double took = now_ms() - start;
  /* about a screenful at 960 characters a second */
  CHECK(r, took > 500, "painted too fast for 9600 baud: %.0f ms", took);
  const char *labels[] = {"PATIENT NAME  :", "TREATMENT MODE: FIX", "BEAM TYPE:", "ENERGY (KeV):",
                          "ACTUAL", "PRESCRIBED", "UNIT RATE/MINUTE", "MONITOR UNITS", "TIME (MIN)",
                          "GANTRY ROTATION (DEG)", "COLLIMATOR ROTATION (DEG)", "COLLIMATOR X (CM)",
                          "COLLIMATOR Y (CM)", "WEDGE NUMBER", "ACCESSORY NUMBER", "DATE  :", "TIME  :",
                          "OPR ID: T25V02-R03", "SYSTEM:", "TREAT :", "REASON: OPERATOR",
                          "OP.MODE: TREAT", "AUTO", "173777", "COMMAND:"};
  for (size_t i = 0; i < sizeof labels / sizeof *labels; i++)
    CHECK(r, on_screen(&s, labels[i]), "missing %s", labels[i]);
  char buf[VT_COLS + 1];
  CHECK(r, strncmp(line(&s.term, 23, buf) + 52, "COMMAND:", 8) == 0, "command line not at lower right");
  /* once a second the clock is rewritten and the cursor makes a trip down there and back */
  for (int i = 0; i < 100 && !(s.term.row == 1 && s.term.col == 18); i++) run_for(&s, 10);
  CHECK(r, s.term.row == 1 && s.term.col == 18, "cursor should wait on the patient name, at %d,%d", s.term.row,
        s.term.col);
  if (r->failure[0]) dump(&s, r);
}

static void normal_treatment(result *r) {
  static session s;
  char buf[64];
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  enter_prescription(&s, 'X');
  CHECK(r, on_screen(&s, "X-RAY"), "beam type not shown");
  for (int i = 0; i < 6; i++) CHECK(r, on_screen(&s, "VERIFIED"), "treatment site not verified");
  CHECK(r, wait_for(&s, "BEAM READY", 15000), "no BEAM READY");
  run_for(&s, 1000);
  CHECK(r, sim_int(&s, RequestPatientDose) == 0, "beam came on without B");
  type(&s, "B\r");
  CHECK(r, finish(&s), "treatment did not finish");
  field(&s, "MONITOR UNITS", buf, sizeof buf);
  CHECK(r, strcmp(buf, "202 202") == 0, "dose monitor should read the prescribed 202 MU: [%s]", buf);
  CHECK(r, sim_int(&s, RequestPatientDose) == 202, "patient dose %d", sim_int(&s, RequestPatientDose));
  if (r->failure[0]) dump(&s, r);
}

/* East Texas Cancer Center: "she had typed 'x' (for X ray) when she had intended 'e' ... she
 * merely used the cursor up key to edit the mode entry ... she hit the return key several times
 * ... the terminal displayed 'beam ready' ... She hit the one-key command 'B'" */
static void tyler(result *r) {
  static session s;
  char buf[64];
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  type(&s, "TEST\r\r");
  double entered = now_ms();
  type(&s, "x\r");
  type(&s, "\r200\r202\r1\r\r\r\r\r\r\r");
  /* past the first magnet (2 s), well inside the 8 s it takes to set them all */
  while (now_ms() < entered + 3000) run_for(&s, 20);
  for (int i = 0; i < 11; i++) press(&s, VK_UP);
  field(&s, "BEAM TYPE:", buf, sizeof buf);
  for (int i = 0; i < 30 && !(s.term.row == 2 && s.term.col == 42); i++) run_for(&s, 10);
  CHECK(r, s.term.row == 2 && s.term.col == 42, "cursor should be on the beam type, at %d,%d", s.term.row,
        s.term.col);
  type(&s, "e\r");
  type(&s, "\r\r\r\r\r\r\r\r\r\r");
  CHECK(r, now_ms() < entered + 7500, "the edit took too long for the race: %.0f ms", now_ms() - entered);
  CHECK(r, on_screen(&s, "BEAM TYPE: E") && on_screen(&s, "ELECTRON"), "the screen should show the edit");
  CHECK(r, wait_for(&s, "BEAM READY", 15000), "no BEAM READY");
  type(&s, "B\r");
  CHECK(r, wait_for(&s, "MALFUNCTION 54", 3000), "no Malfunction 54");
  CHECK(r, on_screen(&s, "TREAT PAUSE"), "should be a treatment pause");
  field(&s, "MONITOR UNITS", buf, sizeof buf);
  CHECK(r, strcmp(buf, "6 6") == 0, "dose monitor should read 6 MU: [%s]", buf);
  CHECK(r, sim_int(&s, RequestPatientDose) >= 16500, "no overdose: %d", sim_int(&s, RequestPatientDose));
  /* "She immediately took the normal action ... which was to hit the 'P' key" */
  type(&s, "P\r");
  run_for(&s, 500);
  CHECK(r, on_screen(&s, "MALFUNCTION 54"), "P should repeat it");
  CHECK(r, sim_int(&s, RequestPatientDose) >= 33000, "second overdose missing");
  if (r->failure[0]) dump(&s, r);
}

/* The same edit, made slowly, is caught; the edit keys also execute B (the FDA's complaint) */
static void slow_edit_is_safe(result *r) {
  static session s;
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  enter_prescription(&s, 'X');
  CHECK(r, wait_for(&s, "BEAM READY", 15000), "no BEAM READY");
  for (int i = 0; i < 11; i++) press(&s, VK_UP);
  type(&s, "e\r\r\r\r\r\r\r\r\r\r\r");
  run_for(&s, 500);
  CHECK(r, !on_screen(&s, "BEAM READY"), "an edit should take BEAM READY away");
  CHECK(r, wait_for(&s, "BEAM READY", 15000), "no BEAM READY after the edit");
  type(&s, "B");
  press(&s, VK_DOWN); /* "the normal edit keys (down arrow ...) will be interpreted as a CR" */
  CHECK(r, finish(&s), "treatment did not finish");
  CHECK(r, sim_int(&s, RequestPatientDose) == 202, "expected a normal 202, got %d",
        sim_int(&s, RequestPatientDose));
  if (r->failure[0]) dump(&s, r);
}

/* Yakima: field light from the hand control, then "set" typed at the console at the moment
 * Class3 rolls over */
static void yakima(result *r) {
  static session s;
  char buf[64];
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  enter_prescription(&s, 'X');
  session_hand(&s, HAND_FIELD_LIGHT);
  CHECK(r, wait_for(&s, "PRESS SET BUTTON", 15000), "no PRESS SET BUTTON");
  double end = now_ms() + 45000; /* Class3 counts ten a second, once set-up test starts */
  int c3 = 0;
  while (now_ms() < end && !((c3 = sim_int(&s, RequestClass3)) >= 244 && c3 <= 249)) run_for(&s, 10);
  CHECK(r, c3 >= 244 && c3 <= 249, "Class3 never came round");
  type(&s, "SET\r");
  CHECK(r, wait_for(&s, "BEAM READY", 5000), "no BEAM READY (Class3 was %d)", c3);
  type(&s, "B\r");
  CHECK(r, wait_for(&s, "FLATNESS", 3000), "no FLATNESS");
  field(&s, "MONITOR UNITS", buf, sizeof buf);
  CHECK(r, strcmp(buf, "0 0") == 0, "\"the console displayed no dose\": [%s]", buf);
  CHECK(r, sim_int(&s, RequestPatientDose) >= 4000, "no overdose");
  if (r->failure[0]) dump(&s, r);
}

/* The page's recipe: nothing but RETURN and cursor up after the X, four keys a second */
static void tyler_with_returns(result *r) {
  static session s;
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  type_at(&s, "TEST\r\r", 250);
  double entered = now_ms();
  type_at(&s, "x\r\r\r\r\r\r\r\r\r\r\r", 250); /* RETURN copies the plan and the room */
  CHECK(r, on_screen(&s, "202") && s.term.row == 23, "RETURNs should reach COMMAND with the plan filled in");
  for (int i = 0; i < 11; i++) {
    press(&s, VK_UP);
    run_for(&s, 220);
  }
  type_at(&s, "e\r", 250);
  CHECK(r, now_ms() < entered + 7500, "the recipe took too long: %.0f ms", now_ms() - entered);
  type_at(&s, "\r\r\r\r\r\r\r\r\r\r", 250); /* back down to COMMAND, no hurry now */
  CHECK(r, wait_for(&s, "BEAM READY", 15000), "no BEAM READY");
  type(&s, "B\r");
  CHECK(r, wait_for(&s, "MALFUNCTION 54", 3000), "no Malfunction 54");
  if (r->failure[0]) dump(&s, r);
}

/* R while the magnets are being set, then the prescription again at once: the machine resets
 * when the magnets are done, and must keep what was typed since */
static void reset_then_reenter(result *r) {
  static session s;
  session_init(&s, 9600, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  type(&s, "TEST\r\rx\r\r\r\r\r\r\r\r\r\r\r");
  run_for(&s, 500);
  type(&s, "R\r");
  type(&s, "\r\rx\r\r\r\r\r\r\r\r\r\r\r");
  CHECK(r, wait_for(&s, "BEAM READY", 25000), "stuck after R and re-entry");
  if (r->failure[0]) dump(&s, r);
}

static void reset_and_verification(result *r) {
  static session s;
  char buf[64];
  session_init(&s, 0, now_ms());
  wait_for(&s, "COMMAND:", 5000);
  type(&s, "TEST\r\rx\r\r200\r202\r1\r");
  type(&s, "90\r"); /* gantry: the room is set to 0 */
  type(&s, "\r\r\r\r\r");
  char row[VT_COLS + 1];
  line(&s.term, 9, row);
  CHECK(r, strstr(row, "GANTRY") && strstr(row, "90") && !strstr(row, "VERIFIED"), "a mismatch must not verify: [%s]",
        row);
  line(&s.term, 10, row);
  CHECK(r, strstr(row, "VERIFIED"), "the copied row should verify: [%s]", row);
  CHECK(r, wait_for(&s, "SET-UP DONE", 15000), "machine should still get to set-up done");
  CHECK(r, !on_screen(&s, "BEAM READY"), "no BEAM READY while unverified");
  int bells = s.term.bells;
  type(&s, "B\r");
  run_for(&s, 300);
  CHECK(r, s.term.bells > bells, "B should ring the bell");
  CHECK(r, sim_int(&s, RequestPatientDose) == 0, "B fired while unverified");
  type(&s, "R\r");
  run_for(&s, 500);
  field(&s, "BEAM TYPE:", buf, sizeof buf);
  CHECK(r, buf[0] == 'E' && buf[1] == 'N', "R should clear the beam type, got [%s]", buf);
  CHECK(r, on_screen(&s, "PATIENT NAME  : TEST"), "R keeps the patient");
  CHECK(r, on_screen(&s, "DATA ENTRY"), "R should go back to data entry");
  if (r->failure[0]) dump(&s, r);
}

typedef struct {
  void (*fn)(result *);
  result res;
} job;

static void *run_job(void *p) {
  job *j = p;
  j->fn(&j->res);
  return NULL;
}

int vt_run_tests(void) {
  job jobs[] = {
      {terminal_tests, {"terminal: control sequences, raster, keyboard", {0}}},
      {paints_figure_a, {"console paints Figure A at 9600 baud", {0}}},
      {normal_treatment, {"normal treatment: BEAM READY waits for B, prescribed MU delivered", {0}}},
      {tyler, {"Tyler: cursor-up edit within 8 s gives MALFUNCTION 54, 6 MU, overdose", {0}}},
      {slow_edit_is_safe, {"a slow edit is caught; B plus down arrow fires", {0}}},
      {yakima, {"Yakima: SET as Class3 rolls over gives FLATNESS, no dose shown", {0}}},
      {reset_and_verification, {"unverified rows block B; R clears the prescription", {0}}},
      {tyler_with_returns, {"Tyler with RETURNs only, at four keys a second", {0}}},
      {reset_then_reenter, {"R during the magnets, then re-entry at once, gets to BEAM READY", {0}}},
  };
  size_t n = sizeof jobs / sizeof *jobs;
  pthread_t th[16];
  for (size_t i = 0; i < n; i++) pthread_create(&th[i], NULL, run_job, &jobs[i]);
  int failures = 0;
  for (size_t i = 0; i < n; i++) {
    pthread_join(th[i], NULL);
    if (jobs[i].res.failure[0]) {
      failures++;
      printf("FAIL %s: %s\n", jobs[i].res.name, jobs[i].res.failure);
    } else {
      printf("ok   %s\n", jobs[i].res.name);
    }
  }
  fflush(stdout);
  return failures;
}
