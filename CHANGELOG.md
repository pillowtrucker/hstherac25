# Revision history for HsTherac25

## Unreleased

* Model checked against Leveson & Turner (1993); see README.md for what is taken from the
  paper and what is assumed.
* Tyler race: the bending-magnet flag is really set, Ptime polls for edits against what Datent
  is setting up, the first magnet is protected, about 8 s in total. Data entry complete is set,
  never toggled, and Begin is needed before any beam.
* Beam-on outcome comes from what the hardware physically does; Malfunction 54 shows 6 MU while
  the patient's real dose is recorded (request 11).
* Yakima: one-byte Class3 incremented on every Set-Up Test pass, F$mal bit 9, field-light
  position, set button (calls 7 and 8), turntable that takes time to move.
* 100 ms scheduler tick, visible treatment suspend after 5 pauses, R works during a pause,
  fewer spurious pauses with the messages operators reported.
* Invalid values from the UI are ignored instead of crashing the host; request 7 works;
  free_state_info; new requests 8-12.
* Scenario test suite, run in CI on Linux.
* VT100 operator console (`vt100/`): the treatment console program with the paper's screen
  layout, a 9600-baud line, and a VT100 emulated from DEC's manuals with its own character
  ROM. It runs in the browser (WebAssembly; CI attaches the site as the `therac25-console`
  artifact, and can publish it on GitHub Pages), on a terminal, or on a real VT100, and has its
  own test suite (`vt100-test`).
* New calls: 9 beam on (B), 10 opt in to B, 11 prescribed monitor units.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
