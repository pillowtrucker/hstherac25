# hstherac25

A simulation of the Therac-25 treatment software, with its two infamous software races
reproduced **on purpose**. It is a shared library with a small C interface (`csrc/Therac.h`)
so that real front ends can be attached to it.

The model follows the most detailed public technical account:
N. G. Leveson and C. S. Turner, *An Investigation of the Therac-25 Accidents*,
IEEE Computer 26(7), July 1993 ([full text](https://www.cs.columbia.edu/~junfeng/08fa-e6998/sched/readings/therac25.pdf),
[DOI 10.1109/MC.1993.274940](https://dl.acm.org/doi/10.1109/MC.1993.274940)).
That paper is itself based on AECL's description for the FDA and says it "leaves some unanswered
questions"; where the simulation has to fill a gap, it is listed under [Assumptions](#assumptions).
Leveson's [2017 follow-up](https://www.computer.org/csdl/magazine/co/2017/11/mco2017110008/13rRUxAStVR)
adds no technical detail.

## The VT100 console

[`vt100/`](vt100/README.md) is a front end that shows the operator's side exactly as far as the
sources allow. It has three parts, all in C:

- the treatment console program, with the paper's screen layout;
- a 9600-baud serial line;
- a DEC VT100, emulated from DEC's manuals and drawing with its own character ROM.

It runs in the browser (compiled to WebAssembly together with this simulator), in your
terminal, or on a real VT100. Both accidents can be reproduced with the historical keystrokes.

## Reproducing the accidents from a UI

**Tyler, 1986 (Malfunction 54).** Enter X-ray and press Begin. Setting the bending magnets
starts when the mode is entered and takes 8 seconds; change the mode to electron after the
first 2 seconds and before the 8 are up (edits during the first magnet are noticed, later ones
are not). The beam fires with X-ray-level current and the turntable in the electron position.
An edit after the 8 seconds is safe, and so is pressing Begin only after them. The console shows `MALFUNCTION 54`, `TP_PauseTreatment`, and a dose
monitor reading of 6 MU. P repeats the overdose each time; the 5th pause suspends treatment.

**Yakima, 1987 (Class3 overflow).** Enter X-ray, press Field light, press Begin. After the
magnets are set (about 8 seconds) the console waits with `PRESS SET BUTTON` while Class3 counts
up ten times a second. Press Set within about 1.5 seconds before Class3 rolls over from 255 to
0. The collimator check is skipped on the pass where Class3 is 0, and the beam fires while the
turntable is still in the field-light position. The console shows `FLATNESS` and no dose,
and P repeats the overdose. Pressed at any other time, Set is safe. Showing the Class3 counter (request 8) in the UI turns
this into a timing game rather than a lottery.

In both cases request 11 reveals the dose the patient actually received, which the real
console could not show.

## Interface

`wrap_external_call(machine, call, beam, collimator, energy)`: the last three arguments only
matter for `SendMEOS`. Values the header doesn't define are ignored.

| # | Call | Meaning |
|---|---|---|
| 1 | `ExtCallSendMEOS` | the prescription on screen changed (beam 1 = X-ray, 2 = electron; collimator 1 = X-ray, 2 = electron, 3 = follow the beam type) |
| 2 | `ExtCallToggleDatentComplete` (alias `ExtCallDataEntryComplete`) | cursor reached the command line ("Begin"). Sets the flag; only a reset clears it |
| 3 | `ExtCallToggleEditingTakingPlace` (alias `ExtCallEditingTakingPlace`) | operator is editing. Sets the flag; `SendMEOS` also sets it |
| 4 | `ExtCallReset` | R |
| 5 | `ExtCallProceed` | P, during a treatment pause |
| 6 | `ExtCallHardReset` | power cycle |
| 7 | `ExtCallSet` | set button on the hand control |
| 8 | `ExtCallFieldLight` | hand control rotates the turntable to the field-light position |
| 9 | `ExtCallBeamOn` | "B" typed at the console: fires the beam once the console says BEAM READY |
| 10 | `ExtCallUseBeamOnKey` | this UI has a "B" command: BEAM READY waits for call 9 instead of firing by itself |
| 11 | `ExtCallPrescribeDose` | prescribed monitor units, in the `beam_energy` argument (default 200) |

`request_state_info(machine, n)` returns a string. Free it with `free_state_info`.

| # | Request | Example |
|---|---|---|
| 1 | treatment outcome | `""`, `TREATMENT OK`, `MALFUNCTION 54`, `FLATNESS`, `H-TILT`, `MALFUNCTION 23` |
| 2 | active subsystem | `DATA ENTRY`, `TREAT` |
| 3 | treatment phase | `TP_Datent`, `TP_SetupTest`, `TP_PauseTreatment`, `TP_TerminateTreatment` |
| 4 | reason | `OPERATOR` or the outcome |
| 5 | beam type the hardware is set up for | `BeamTypeXRay` |
| 6 | energy the hardware is set up for | `25000` |
| 7 | full state dump | `TheracState {...}` |
| 8 | Class3 | `0` to `255` |
| 9 | turntable position | `CollimatorPositionFieldLight` |
| 10 | dose monitor reading for the last attempt (MU) | `6` |
| 11 | dose the patient actually received since the last reset (rads) | `17544` |
| 12 | set prompt | `PRESS SET BUTTON` or `""` |

`TP_TerminateTreatment` with an outcome other than `TREATMENT OK` means treatment suspend;
press R. After `kill_machine` the Haskell runtime cannot be started again in the same process,
so use call 6 to restart a simulation.

## What comes from the paper

- Tasks: Treat runs the eight Tphase subroutines and reschedules itself every 0.1 s; the
  keyboard handler and the housekeeper run concurrently and talk to it through shared variables.
- Datent, Magnet and Ptime follow the paper's Figure 3. Ptime clears the bending-magnet flag at
  the end of its *first* call, so only edits during the first magnet are noticed. Setting the
  magnets takes about 8 s.
- The data-entry-complete flag "only indicates that the cursor has been down to the command
  line, not that it is still there", so it is set and never cleared by editing.
- Hand moves the turntable to follow MEOS; Datent sets the beam. "The software appears to include
  no checks to detect such an incompatibility." The outcome of a beam-on is decided by what the
  hardware physically does, not by the software comparing anything.
- Malfunction 54 was "dose input 2", a dose "either too high or too low". The saturated ion
  chamber showed an underdose: 6 of 202 MU. Simulated doses: 16,500 to 25,000 rads.
- Set-Up Test increments the one-byte Class3 on every pass, then proceeds when F$mal is zero.
  The housekeeper's Lmtchk calls Chkcol only when Class3 is nonzero; Chkcol sets bit 9 of F$mal
  when the upper collimator is inconsistent with the treatment. The operator hit set "at the
  precise moment that Class3 rolled over to zero". Yakima showed no dose, then "flatness" on
  the reason line. Doses of 4,000 to 5,000 rads per attempt.
- Treatment pause allows P, up to five pauses; then treatment suspend requires a reset.
- Spurious pauses show the kinds of messages operators reported ("low dose rate, V-tilt, H-tilt",
  "malfunction" plus a number from 1 to 64). X-ray mode needs about 100 times the electron
  current, and a typical dose is about 200 rads.

## Assumptions

These are not in the paper, or the paper is ambiguous. They are chosen to keep the lessons of the
two accidents intact and easy to reproduce.

- **Slow edits are safe.** The paper says "data-entry speed during editing was the key factor" but
  not what caught slow edits. Here an edit made after Datent has finished (during Set-Up Test or
  a pause) sends the machine back through Datent.
- **F$mal is rebuilt on every housekeeper pass**, so a bypassed Chkcol leaves bit 9 clear. The
  paper only says Chkcol "sets or resets" bit 9 and that when it was bypassed F$mal "was not set".
- **The turntable takes 2 seconds to move.** The paper gives no figure. This is the width of the
  Yakima window: Set must come less than about 2 s before Class3 rolls over.
- **The turntable only moves during data entry and set-up**, not once set-up is done or during a
  pause. So P after a Yakima overdose repeats it, as the paper describes ("paused again, this time
  displaying 'flatness'"; "After two attempts, the patient could have received 8,000 to 10,000").
- **Four magnets of 2 seconds each.** The paper only says "several magnets" and "about 8 seconds".
- **The housekeeper runs every 0.05 s**, twice per Treat tick, so it always sees Class3 between two
  Set-Up Test passes. The paper says tasks are initiated every 0.1 s.
- **Begin doubles as the "B" (beam on) key**: after Set-Up Done the beam fires without a separate
  command, unless the UI has opted in to a real B with call 10 (the VT100 console does).
- **The editing flag stays set** once the prescription has been edited. On the PDP-11 an edit took
  keystrokes, long enough for Ptime to see the flag; a UI can send an edit and "back to the
  command line" in the same millisecond.
- **A 30% chance of a spurious pause** on a correct setup. The paper reports frequent malfunctions
  but gives no rate.
- An electron beam with the wrong accessories in place is treated as a harmless nuisance pause.
- Field light pressed during a pause goes back to Set-Up Test (at Yakima the operator used it
  between exposures).
- Class3 and the turntable position survive a soft reset. Hard reset re-initialises everything.

## Building and testing

```
cabal build all
cabal test
```

`HsTherac25-test` runs every scenario above against its own machine, all at once, in about 40 s.
It checks that both accidents happen when provoked the historical way, and that the paths the
original software got right stay safe. `vt100-test` does the same through the VT100 console,
keystroke by keystroke. `vt100/README.md` covers the browser build.
