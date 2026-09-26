/* The Therac-25 treatment console program: the part of the PDP-11 software the operator
 * talked to through the VT100 (keyboard handler, the data-entry screen, the treatment console
 * screen processor). It only sees bytes from the terminal and writes bytes back; the machine
 * is the hstherac25 simulator, reached through csrc/Therac.h. */
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "Therac.h"

typedef void (*console_out_fn)(void *ctx, const uint8_t *buf, size_t n);

enum console_field {
  F_NAME,
  F_MODE,
  F_BEAM,
  F_ENERGY,
  F_RATE,
  F_MU,
  F_TIME,
  F_GANTRY,
  F_COLLROT,
  F_COLLX,
  F_COLLY,
  F_WEDGE,
  F_ACCESSORY,
  F_COMMAND,
  F_COUNT
};

#define CONSOLE_SITE_ROWS 6

typedef struct console {
  HsStablePtr machine;
  console_out_fn out;
  void *out_ctx;

  /* the form */
  char text[F_COUNT][24];      /* what the field shows */
  char committed[F_COUNT][24]; /* its last accepted value (for cursor-up without accepting) */
  bool fresh;                  /* the next printable key replaces the field */
  int field;                   /* where the cursor is */
  double actual[CONSOLE_SITE_ROWS]; /* treatment site as set up in the room */
  char beam;                   /* 'X', 'E' or 0: accepted beam type */
  int energy;                  /* accepted MeV, 0 if none */
  char cmd[12];

  /* keyboard handler: escape sequences from the terminal */
  int esc_state;

  /* what the machine said at the last screen processor pass */
  char outcome[64], phase[32], reason[64], set_prompt[32];
  int displayed_mu;

  /* screen processor */
  char shown[24][80]; /* what the terminal is showing, as far as we know */
  int cur_row, cur_col;
  bool need_clear;
  double next_pass;
  long long clock_s; /* local time, seconds since 1970 */
} console;

void console_init(console *c, HsStablePtr machine, console_out_fn out, void *ctx);
/* one byte from the terminal */
void console_input(console *c, uint8_t byte);
/* runs the screen processor every 0.1 s; local_epoch_s is the wall clock in local time */
void console_tick(console *c, double now_ms, long long local_epoch_s);
/* the machine was power cycled: clear the form and repaint */
void console_power_cycle(console *c);
/* the 24 lines the program wants on the screen (for tests); out must hold 24*81+1 bytes */
void console_wanted_text(console *c, char *out);
