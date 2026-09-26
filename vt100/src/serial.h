/* One direction of an asynchronous serial line: 8 data bits, no parity, 1 stop bit, so a
 * character takes 10 bit times. A byte put on the line arrives when its last bit has. */
#pragma once
#include <stdbool.h>
#include <stdint.h>

#define SERIAL_BUF 32768

typedef struct {
  uint8_t buf[SERIAL_BUF];
  unsigned head, tail; /* head: next to arrive, tail: next free */
  double char_ms;      /* 0 = infinitely fast */
  double busy_until;   /* when the character now on the wire has arrived */
  bool paused;         /* flow control (XOFF): the sender stops after the current character */
} serial_line;

void serial_init(serial_line *l, int baud);
bool serial_put(serial_line *l, uint8_t b, double now_ms);
/* next byte that has arrived by now_ms, or -1 */
int serial_get(serial_line *l, double now_ms);
bool serial_empty(const serial_line *l);
/* XOFF / XON */
void serial_set_paused(serial_line *l, bool paused, double now_ms);
