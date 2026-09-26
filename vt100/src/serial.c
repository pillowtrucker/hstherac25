#include "serial.h"

void serial_init(serial_line *l, int baud) {
  l->head = l->tail = 0;
  l->char_ms = baud > 0 ? 10.0 * 1000.0 / baud : 0.0;
  l->busy_until = 0;
  l->paused = false;
}

bool serial_empty(const serial_line *l) { return l->head == l->tail; }

bool serial_put(serial_line *l, uint8_t b, double now_ms) {
  unsigned next = (l->tail + 1) % SERIAL_BUF;
  if (next == l->head) return false;
  if (serial_empty(l) && l->busy_until < now_ms) l->busy_until = now_ms + l->char_ms; /* line idle */
  l->buf[l->tail] = b;
  l->tail = next;
  return true;
}

int serial_get(serial_line *l, double now_ms) {
  if (serial_empty(l) || l->busy_until > now_ms) return -1;
  int b = l->buf[l->head];
  l->head = (l->head + 1) % SERIAL_BUF;
  if (!serial_empty(l)) {
    if (l->paused) {
      l->busy_until = 1e300; /* resumes on XON */
    } else {
      l->busy_until += l->char_ms;
    }
  }
  return b;
}

void serial_set_paused(serial_line *l, bool paused, double now_ms) {
  l->paused = paused;
  if (!paused && l->busy_until > 1e299) l->busy_until = now_ms + l->char_ms;
}
