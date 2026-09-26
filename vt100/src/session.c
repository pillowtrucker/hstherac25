#include "session.h"

static void to_terminal(void *ctx, const uint8_t *buf, size_t n) {
  session *s = ctx;
  for (size_t i = 0; i < n; i++) serial_put(&s->to_term, buf[i], s->now);
}

void session_init(session *s, int baud, double now_ms) {
  s->now = now_ms;
  s->machine = start_machine();
  vt_init(&s->term);
  serial_init(&s->to_term, baud);
  serial_init(&s->to_host, baud);
  console_init(&s->con, s->machine, to_terminal, s);
}

void session_step(session *s, double now_ms, long long local_epoch_s) {
  uint8_t buf[64];
  int b, n;
  s->now = now_ms;
  vt_tick(&s->term, now_ms);
  while ((n = vt_take_tx(&s->term, buf, sizeof buf)) > 0)
    for (int i = 0; i < n; i++) serial_put(&s->to_host, buf[i], now_ms);
  while ((b = serial_get(&s->to_host, now_ms)) >= 0) {
    if (b == 0x13) serial_set_paused(&s->to_term, true, now_ms); /* NO SCROLL: XOFF */
    else if (b == 0x11) serial_set_paused(&s->to_term, false, now_ms);
    else console_input(&s->con, (uint8_t)b);
  }
  console_tick(&s->con, now_ms, local_epoch_s);
  while ((b = serial_get(&s->to_term, now_ms)) >= 0) vt_receive(&s->term, (uint8_t)b);
}

void session_key(session *s, int key, bool down, double now_ms) {
  vt_key(&s->term, key, down, now_ms);
}

void session_hand(session *s, int button) {
  wrap_external_call(s->machine, button == HAND_SET ? ExtCallSet : ExtCallFieldLight, BTCheekyPadding,
                     CPCheekyPadding, 0);
}

/* the Therac-25 is power cycled; the VT100 on the desk is not */
void session_power_cycle(session *s) { console_power_cycle(&s->con); }
