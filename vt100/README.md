# The Therac-25 console on a VT100

This is the operator's side of the Therac-25, rebuilt end to end:

- **the treatment console program** on the PDP-11/23 (`src/console.c`), which lays out the
  data-entry screen, reads the keyboard and talks to the machine;
- **a serial line** at 9600 baud (`src/serial.c`), so the screen paints as fast as a real one
  could;
- **a DEC VT100** (`src/vt100.c`), emulated from DEC's manuals, drawing its characters from
  the VT100's own character-generator ROM.

The machine is the hstherac25 simulator, called through the same C interface
(`csrc/Therac.h`) as every other front end. In the browser it all runs in one WebAssembly
module: the C code and the Haskell simulator, compiled by GHC's WebAssembly backend. The
simulator's Treat, housekeeper and Ptime tasks run on the page's event loop.

```
 browser page (main.js): CRT shader, keyboard, sound, hand control
      │ keys                              ▲ 800 x 240 dots
      ▼                                   │
 vt100.c  VT100 ──── serial.c 9600 baud ──── console.c  Therac console ── Therac.h ── HsTherac25
```

## Running it

**In a browser.** Every CI run of `.github/workflows/web.yml` attaches the finished site as the
artifact `therac25-console`: a folder of static files that works on any web server (it has to be
served over HTTP; opening `index.html` from disk won't load the module). The page names no
repository unless the repository variable `SOURCE_URL` is set. Set `DEPLOY_PAGES` to `true` (and
Settings → Pages → Source: GitHub Actions) to also publish pushes to `main` on GitHub Pages.

To build it yourself you need GHC's WebAssembly toolchain,
[ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta), flavour 9.12
(`SOURCE_URL=... vt100/build-web.sh` for a footer link to the source):

```
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.12 sh
source ~/.ghc-wasm/env
wasm32-wasi-cabal update
vt100/build-web.sh
python3 -m http.server -d vt100/dist      # then open http://localhost:8000
node vt100/test/smoke.mjs                 # the Tyler accident, typed into the wasm build
```

**On a real terminal.** `cabal run therac-vt100` runs the console program in your terminal,
paced to 9600 baud (`--baud 0` for full speed). It only sends VT100 control sequences, so it
also drives a real VT100:
`stty -F /dev/ttyS0 9600 raw; therac-vt100 < /dev/ttyS0 > /dev/ttyS0`. The hand control is a
pair of signals: `kill -USR1 <pid>` for field light, `kill -USR2 <pid>` for the set button.

**Tests.** `cabal test vt100-test` checks the terminal (control sequences, dot stretcher,
underline, auto repeat, keyboard codes). It then plays the historical keystrokes through the
whole chain against the real simulator: Tyler, a slow edit that is caught, Yakima, a normal
treatment, unverified rows and reset. The scenarios run in parallel and take about 40 s.

## Using the console

The screen is Figure A of the paper. The cursor starts on PATIENT NAME.

- Type a value and press RETURN to accept it and move to the next field. On an empty
  treatment-site field (gantry, collimator, wedge, accessory), RETURN copies the value set up in
  the room, and the row shows VERIFIED.
- Cursor up (↑) moves back one field. BACK SPACE, DELETE and ← rub out.
- Down arrow, right arrow and LINE FEED act as RETURN.
- The last RETURN puts the cursor on COMMAND. Type a command, then RETURN:
  - `B` beam on, once SYSTEM says BEAM READY;
  - `P` proceed after a treatment pause;
  - `R` reset (clears the prescription);
  - `SET` the set button.
- The hand control in the treatment room has FIELD LIGHT and SET buttons.

On a PC keyboard: Enter is RETURN, Backspace is BACK SPACE, Delete is DELETE, Insert is LINE
FEED, F1–F4 are PF1–PF4, Scroll Lock is NO SCROLL, the numeric keypad is the keypad. The
drawn keyboard works with a mouse or a finger. SHIFT and CTRL stay down until the next key.

"What the operator couldn't see" shows the simulator's state: Tphase, Class3, where the
turntable is, what the beam is set up for, and the dose the patient really received.

The steps for both accidents are on the page and in the main [README](../README.md).

## Where each detail comes from

From N. G. Leveson and C. S. Turner, "An Investigation of the Therac-25 Accidents", IEEE
Computer 26(7), July 1993:

- "The Therac-25 operator controls the machine with a DEC VT100 terminal."
- **The screen layout** is Figure A. The labels, and the example values `A  1`, `AUTO`,
  `173777` and `OPR ID: T25V02-R03`, are copied from it, including `ENERGY (KeV):` for a value
  in MeV. The monospaced HTML version of the figure pads the labels so their colons line up.
- **VERIFIED:** "The system then compares the manually set values with those entered at the
  console. If they match, a 'verified' message is displayed and treatment is permitted. If they
  do not match, treatment is not allowed to proceed".
- **Copying with RETURN:** "operators could use a carriage return to merely copy the treatment
  site data. A quick series of carriage returns would thus complete data entry."
- **The Tyler keystrokes:** "she had typed 'x' (for X ray) when she had intended 'e' ... she
  merely used the cursor up key to edit the mode entry ... she hit the return key several times
  and left their values unchanged. She reached the bottom of the screen where a message
  indicated that the parameters had been 'verified' and the terminal displayed 'beam ready' ...
  She hit the one-key command 'B'."
- **The command line:** "The command line at the lower right corner of the screen is the
  cursor's normal position when the operator has completed all necessary changes to the
  prescription. Prescription editing is signified by cursor movement off the command line."
- **Default energy:** "the data-entry process forces the operator to enter the mode and energy,
  except when the operator selects the photon mode, in which case the energy defaults to 25
  MeV."
- **Cursor up:** "the key used for moving the cursor back through the prescription sequence
  (i.e., cursor 'UP' inscribed with an upward pointing arrow)". R and re-entry: "an 'R' reset
  command must be used and the whole prescription reentered."
- **The FDA's complaint:** "the normal edit keys (down arrow, right arrow, or line feed) will be
  interpreted as a CR and initiate exposure. One must use either the backspace or left arrow
  key to edit."
- **Yakima:** "The console displays the message 'Press set button' while the turntable is in
  the field-light position. The operator now presses the set button on the hand control or
  types 'set' at the console." "the console displayed no dose or dose rate", then "flatness" on
  the reason line.
- **Malfunction 54 and the pause:** "Malfunction 54", "treatment pause", "6 monitor units
  delivered, whereas the operator had requested 202", and "P" to proceed.
- **The screen processor:** "Treatment console screen processor (run periodically)", with tasks
  scheduled every 0.1 s.

From DEC's VT100 User Guide (EK-VT100-UG) and Technical Manual (EK-VT100-TM), as published on
[vt100.net](https://vt100.net/):

- **The raster:** 24 × 80, 800 dots × 240 scans, 10 × 10 cells. A 12-inch P4 tube with a
  202 × 115 mm active area.
- **The dot stretcher:** "delaying the VIDEO IN H signal by one dot time ... and then ORing the
  undelayed and delayed signals". The last dot of a ROM row is replicated across the cell.
- **Attributes:** four levels (black, dim, normal, bright). Underline on the ninth scan.
  Reverse characters on a dim background. Blinking at about 0.5 Hz, half the cursor's rate.
  Blinking block or blinking underline cursor.
- **Keyboard codes:** ESC [ A etc., or ESC O A in cursor key mode; BS, DEL, LF, CR. The key
  positions are taken from DEC's drawing.
- **Auto repeat:** about half a second, then about 30 a second. RETURN, ESC, SET-UP,
  NO SCROLL, BREAK and ENTER never repeat.
- **Sound:** the keyclick, and "an 800 hertz tone ... for 0.25 seconds" for the bell.

The glyphs are the contents of the VT100 character-generator ROM, DEC part 23-018E2, from
the dump published by [PCjs](https://www.pcjs.org/machines/dec/vt100/rom/).
`tools/mkfont.py` converts it into `src/vt100_font.inc`.

## Assumptions

Neither the paper nor DEC says these:

- **Column positions and line spacing.** Figure A is typeset in a proportional font, so the
  columns are scaled from it onto 80 columns. The status block is on the bottom three lines,
  because the command line is "at the lower right corner of the screen".
- **The status wording**, other than BEAM READY and TREAT PAUSE.
  - TREAT shows the Treat phase with the names from the paper's Figure 2 (DATA ENTRY,
    SET-UP TEST, SET-UP DONE, TREAT ON, TREAT PAUSE, TREAT SUSPEND, TERMINATE).
  - SYSTEM shows PRESS SET BUTTON, BEAM READY or BEAM ON.
  - REASON shows the simulator's reason (OPERATOR, MALFUNCTION 54, FLATNESS, ...).
- **Commands end with RETURN.** The narrative calls B a "one-key command", but the FDA letter
  talks about "entering the 'B' (beam on) code but before the CR is pressed".
- **R clears the prescription** (beam type down to accessory) and keeps the patient's name.
- **The VERIFIED tolerance** is 1 degree, 0.5 cm, and exact for wedge and accessory numbers.
  An unverified row is blank, and B rings the bell until every row verifies. Figure A verifies
  14.3 against 14.2, so there was some tolerance.
- **The room set-up** is Figure A's (gantry 0.0, collimator 359.2 / 14.2 / 27.2, wedge 1,
  accessory 0). TREATMENT MODE is filled in as FIX.
- **Anything typed and not accepted with RETURN** is dropped when the cursor moves up.
- **The meaning of `A  1`, `AUTO` and `173777`** is unknown; they are shown as in the figure.
- **The line runs at 9600 baud.** The paper doesn't give the rate.
- **SET-UP:** a blinking block cursor (switchable), auto wrap off, keyclick on. SET-UP mode
  itself, smooth scroll, 132 columns and VT52 mode are not emulated.
- **DATE and TIME** come from your clock, in the figure's format (`84-OCT-26`, `12:55. 8`).
- **The dose display:** the simulator delivers a treatment at once, so ACTUAL UNIT RATE stays 0,
  and ACTUAL TIME is the monitor units divided by the prescribed rate.
- **Simulator additions** for this console (`csrc/Therac.h`, calls 9–11):
  - an explicit beam-on (B) that BEAM READY waits for, opted into with call 10 so older UIs
    keep "Begin doubles as B";
  - the prescribed monitor units, so a normal treatment shows 202 when 202 was prescribed.

## Files

| | |
|---|---|
| `src/console.c` | the Therac console program: form, keyboard handler, screen processor |
| `src/vt100.c` | the terminal: parser, screen, raster, keyboard, auto repeat, bell |
| `src/vt100_font.inc` | the VT100 character ROM (generated by `tools/mkfont.py`) |
| `src/serial.c` | the serial line |
| `src/session.c` | ties machine, console, line and terminal together (browser and tests) |
| `src/web_main.c`, `hs/WebMain.hs` | the WebAssembly entry points |
| `src/native_tty.c`, `hs/NativeMain.hs` | the console on a real terminal |
| `web/` | the page: `therac.mjs` loads the module, `main.js` draws and listens |
| `test/tests.c` | `cabal test vt100-test` |
| `test/smoke.mjs` | the wasm build in node |
