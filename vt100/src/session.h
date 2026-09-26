/* The whole installation: the simulated machine, the console program on the PDP-11, the serial
 * line and the VT100 on the operator's desk. Used by the web build and by the tests. */
#pragma once
#include "console.h"
#include "serial.h"
#include "vt100.h"

enum { HAND_FIELD_LIGHT, HAND_SET };

typedef struct session {
  HsStablePtr machine;
  vt100 term;
  serial_line to_term, to_host;
  console con;
  double now;
} session;

/* baud 0: characters arrive instantly */
void session_init(session *s, int baud, double now_ms);
void session_step(session *s, double now_ms, long long local_epoch_s);
void session_key(session *s, int key, bool down, double now_ms);
/* the hand control in the treatment room talks to the machine, not to the console */
void session_hand(session *s, int button);
void session_power_cycle(session *s);
