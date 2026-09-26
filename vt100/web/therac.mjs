// Loads the Therac-25 console (therac.wasm) and wraps its entry points (vt100/src/web_main.c).
// No DOM here: the page (main.js) and the node smoke test (../test/smoke.mjs) both use it.
import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/browser_wasi_shim/index.js";
import jsffi from "./ghc_wasm_jsffi.js";

export const DOTS = 800; // "There are 800 dots in a scan
export const SCANS = 240; // and the raster is made of 240 scans"

// enum vt_key in vt100/src/vt100.h. Typewriter keys are their unshifted character code.
const VK_BASE = 0x100;
export const VK = Object.freeze(
  Object.fromEntries(
    [
      "RETURN", "LINEFEED", "BACKSPACE", "DELETE", "TAB", "ESC", "BREAK", "NOSCROLL", "SETUP",
      "UP", "DOWN", "LEFT", "RIGHT", "PF1", "PF2", "PF3", "PF4",
      "KP0", "KP1", "KP2", "KP3", "KP4", "KP5", "KP6", "KP7", "KP8", "KP9",
      "KP_MINUS", "KP_COMMA", "KP_PERIOD", "KP_ENTER", "SHIFT", "CTRL", "CAPSLOCK",
    ].map((name, i) => [name, VK_BASE + i]),
  ),
);

export const HAND_FIELD_LIGHT = 0;
export const HAND_SET = 1;

// csrc/Therac.h StateInfoRequest
export const REQUEST = Object.freeze({
  outcome: 1, subsystem: 2, phase: 3, reason: 4, hardwareBeam: 5, hardwareEnergy: 6, dump: 7,
  class3: 8, turntable: 9, displayedDose: 10, patientDose: 11, setPrompt: 12, magnet: 13,
  bendingMagnetFlag: 14,
});

// local wall-clock time in seconds, for the DATE and TIME fields
export function localEpochSeconds() {
  const now = Date.now();
  return now / 1000 - new Date(now).getTimezoneOffset() * 60;
}

export async function loadTherac(wasm) {
  // argv[0] for the Haskell runtime, which the JSFFI constructor starts from _initialize
  const wasi = new WASI(["therac"], [], [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((line) => console.log(line)),
    ConsoleStdout.lineBuffered((line) => console.warn(line)),
  ], { debug: false });
  // the JSFFI glue needs the instance's exports, which only exist after instantiation
  const exports = {};
  const imports = { wasi_snapshot_preview1: wasi.wasiImport, ghc_wasm_jsffi: jsffi(exports) };
  let instance;
  if (typeof Response !== "undefined" && wasm instanceof Response) {
    const bytes = await wasm.arrayBuffer();
    ({ instance } = await WebAssembly.instantiate(bytes, imports));
  } else {
    ({ instance } = await WebAssembly.instantiate(wasm, imports));
  }
  Object.assign(exports, instance.exports);
  wasi.initialize(instance); // runs the constructors, which start the Haskell runtime
  exports.therac_boot();
  return new Therac(exports);
}

class Therac {
  constructor(x) {
    this.x = x;
    this.decoder = new TextDecoder();
  }
  start(now) {
    this.x.web_init(now);
  }
  // advances everything to `now`; true if the raster was redrawn
  frame(now) {
    return this.x.web_frame(now, localEpochSeconds()) !== 0;
  }
  // 800 x 240 dots, intensity 0 (black) to 3 (bright)
  raster() {
    return new Uint8Array(this.x.memory.buffer, this.x.web_raster(), DOTS * SCANS);
  }
  key(key, down, now) {
    this.x.web_key(key, down ? 1 : 0, now);
  }
  hand(button) {
    this.x.web_hand(button);
  }
  powerCycle() {
    this.x.web_power();
  }
  cursorStyle(block) {
    this.x.web_cursor_style(block ? 1 : 0);
  }
  leds() {
    return this.x.web_leds();
  }
  takeClicks() {
    return this.x.web_take_clicks();
  }
  takeBells() {
    return this.x.web_take_bells();
  }
  // the 24 lines on the screen
  text() {
    return this.cstring(this.x.web_text());
  }
  // a csrc/Therac.h state request (REQUEST above)
  info(request) {
    return this.cstring(this.x.web_info(request));
  }
  cstring(ptr) {
    const mem = new Uint8Array(this.x.memory.buffer);
    let end = ptr;
    while (mem[end]) end++;
    return this.decoder.decode(mem.subarray(ptr, end));
  }
}
