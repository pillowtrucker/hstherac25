/* The console program on a real terminal: an xterm, or a VT100 on a serial port
 * (therac-vt100 < /dev/ttyS0 > /dev/ttyS0 after stty 9600 raw). The hand control in the
 * treatment room is two signals: SIGUSR1 = field light, SIGUSR2 = set button. */
#define _DEFAULT_SOURCE
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "console.h"
#include "serial.h"

static serial_line line; /* pacing, so an xterm draws as slowly as a 9600-baud VT100 */
static double now;
static volatile sig_atomic_t quit, field_light, set_button;

static double now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

static void out(void *ctx, const uint8_t *buf, size_t n) {
  (void)ctx;
  for (size_t i = 0; i < n; i++) serial_put(&line, buf[i], now);
}

static void on_signal(int sig) {
  if (sig == SIGUSR1) field_light = 1;
  else if (sig == SIGUSR2) set_button = 1;
  else quit = 1;
}

int therac_native_main(int baud) {
  struct termios saved, raw;
  if (tcgetattr(STDIN_FILENO, &saved) != 0) {
    fprintf(stderr, "therac-vt100: standard input is not a terminal\n");
    return 1;
  }
  fprintf(stderr,
          "therac-vt100: hand control in the treatment room: kill -USR1 %d (field light), "
          "kill -USR2 %d (set button). Ctrl-C quits.\n",
          (int)getpid(), (int)getpid());
  raw = saved;
  raw.c_iflag &= (tcflag_t) ~(IXON | IXOFF | ICRNL | INLCR | IGNCR | ISTRIP | BRKINT | PARMRK);
  raw.c_oflag &= (tcflag_t)~OPOST;
  raw.c_lflag &= (tcflag_t) ~(ECHO | ICANON | IEXTEN);
  raw.c_cflag |= CS8;
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 0;
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

  struct sigaction sa;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = on_signal;
  sigaction(SIGINT, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);
  sigaction(SIGUSR1, &sa, NULL);
  sigaction(SIGUSR2, &sa, NULL);

  now = now_ms();
  serial_init(&line, baud);
  HsStablePtr machine = start_machine();
  static console con;
  console_init(&con, machine, out, NULL);

  while (!quit) {
    struct pollfd p = {STDIN_FILENO, POLLIN, 0};
    poll(&p, 1, 5);
    now = now_ms();
    uint8_t in[64];
    ssize_t n = (p.revents & POLLIN) ? read(STDIN_FILENO, in, sizeof in) : 0;
    for (ssize_t i = 0; i < n; i++) {
      if (in[i] == 0x13) serial_set_paused(&line, true, now); /* NO SCROLL */
      else if (in[i] == 0x11) serial_set_paused(&line, false, now);
      else console_input(&con, in[i]);
    }
    if (field_light) {
      field_light = 0;
      wrap_external_call(machine, ExtCallFieldLight, BTCheekyPadding, CPCheekyPadding, 0);
    }
    if (set_button) {
      set_button = 0;
      wrap_external_call(machine, ExtCallSet, BTCheekyPadding, CPCheekyPadding, 0);
    }
    time_t t = time(NULL);
    struct tm lt;
    localtime_r(&t, &lt);
    console_tick(&con, now, (long long)t + lt.tm_gmtoff);
    uint8_t buf[512];
    size_t k = 0;
    int b;
    while (k < sizeof buf && (b = serial_get(&line, now)) >= 0) buf[k++] = (uint8_t)b;
    if (k && write(STDOUT_FILENO, buf, k) < 0) break;
  }
  const char bye[] = "\033[H\033[2J";
  if (write(STDOUT_FILENO, bye, sizeof bye - 1) < 0) { /* nothing to do */ }
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &saved);
  return 0;
}
