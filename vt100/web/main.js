// The page around the VT100: draws the 800 x 240 raster on a CRT, turns the PC keyboard (or
// taps on the drawn one) into VT100 key presses, plays the bell and keyclick, and wires the
// treatment-room hand control. The terminal, the console program and the machine are all in
// therac.wasm.
import { loadTherac, VK, DOTS, SCANS, REQUEST, HAND_FIELD_LIGHT, HAND_SET } from "./therac.mjs";

const canvas = document.getElementById("screen");
const statusLine = document.getElementById("status");
const mirror = document.getElementById("screen-text");

// ---------------------------------------------------------------- CRT

// The raster is 202 x 115 mm ("Active Display Size", VT100 User Guide) on a 12-inch 4:3 tube.
const RASTER_ON_GLASS = [202 / 244, 115 / 183];
// P4 phosphor: white with a blue cast
const P4 = [0.8, 0.88, 1.0];
// the video processor's four levels: black, dim, normal, bright
const LEVELS = [0, 0.42, 0.78, 1.0];

const VERTEX = `#version 300 es
in vec2 pos;
out vec2 uv;
void main() { uv = pos * 0.5 + 0.5; gl_Position = vec4(pos, 0.0, 1.0); }`;

const FRAGMENT = `#version 300 es
precision highp float;
uniform sampler2D raster;
uniform vec2 rasterOnGlass;
uniform vec3 phosphor;
uniform vec4 levels;
in vec2 uv;
out vec4 color;

float dotAt(int x, int y) {
  if (x < 0 || x >= ${DOTS} || y < 0 || y >= ${SCANS}) return 0.0;
  int v = int(texelFetch(raster, ivec2(x, y), 0).r * 255.0 + 0.5);
  return v == 0 ? 0.0 : v == 1 ? levels.y : v == 2 ? levels.z : levels.w;
}

void main() {
  vec2 p = uv * 2.0 - 1.0;
  p *= 1.0 + 0.03 * dot(p, p);                 // a slightly curved tube
  vec2 r = (p / rasterOnGlass) * 0.5 + 0.5;    // 0..1 across the raster
  float X = r.x * ${DOTS}.0;
  float Y = (1.0 - r.y) * ${SCANS}.0;
  int dx = int(floor(X));
  int sy = int(floor(Y));

  // each scan is a beam spot: Gaussian across the line, so the gaps between scans show;
  // brighter spots are a little bigger
  float beam = 0.0;
  for (int j = -1; j <= 1; j++) {
    int s = sy + j;
    float d_s = Y - (float(s) + 0.5);
    for (int i = -2; i <= 2; i++) {
      int d = dx + i;
      float L = dotAt(d, s);
      if (L == 0.0) continue;
      float d_d = X - (float(d) + 0.5);
      float sv = 0.24 + 0.08 * L;
      float sh = 0.55 + 0.12 * L;
      beam += L * exp(-d_s * d_s / (2.0 * sv * sv)) * exp(-d_d * d_d / (2.0 * sh * sh)) / (2.5066 * sh);
    }
  }

  // halation: light scattered in the glass around bright areas
  float glow = 0.0;
  for (int j = -2; j <= 2; j++)
    for (int i = -6; i <= 6; i += 2)
      glow += dotAt(dx + i, sy + j * 2);
  glow /= 35.0;

  float I = beam * 1.15 + glow * 0.35;
  vec3 c = phosphor * (1.0 - exp(-1.6 * I));
  vec2 v = uv * 2.0 - 1.0;
  float vignette = 1.0 - 0.18 * dot(v, v);
  vec3 glass = vec3(0.022, 0.026, 0.027) * vignette;
  color = vec4(glass + c * vignette, 1.0);
}`;

function makeGL(canvas) {
  const gl = canvas.getContext("webgl2", { antialias: false, alpha: false, preserveDrawingBuffer: true });
  if (!gl) return null;
  const shader = (type, src) => {
    const s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
    return s;
  };
  const prog = gl.createProgram();
  gl.attachShader(prog, shader(gl.VERTEX_SHADER, VERTEX));
  gl.attachShader(prog, shader(gl.FRAGMENT_SHADER, FRAGMENT));
  gl.linkProgram(prog);
  if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(prog));
  gl.useProgram(prog);
  const buf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buf);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW);
  const loc = gl.getAttribLocation(prog, "pos");
  gl.enableVertexAttribArray(loc);
  gl.vertexAttribPointer(loc, 2, gl.FLOAT, false, 0, 0);
  const tex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, tex);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.R8, DOTS, SCANS, 0, gl.RED, gl.UNSIGNED_BYTE, null);
  gl.uniform2f(gl.getUniformLocation(prog, "rasterOnGlass"), ...RASTER_ON_GLASS);
  gl.uniform3f(gl.getUniformLocation(prog, "phosphor"), ...P4);
  gl.uniform4f(gl.getUniformLocation(prog, "levels"), ...LEVELS);
  return {
    upload(raster) {
      gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, DOTS, SCANS, gl.RED, gl.UNSIGNED_BYTE, raster);
    },
    draw() {
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    },
  };
}

// without WebGL2: every scan drawn as a lit line and a dark one
function make2D(canvas) {
  const ctx = canvas.getContext("2d");
  const off = document.createElement("canvas");
  off.width = DOTS;
  off.height = SCANS * 2;
  const octx = off.getContext("2d");
  const img = octx.createImageData(DOTS, SCANS * 2);
  return {
    upload(raster) {
      const d = img.data;
      for (let y = 0; y < SCANS; y++)
        for (let x = 0; x < DOTS; x++) {
          const L = LEVELS[raster[y * DOTS + x]];
          for (let k = 0; k < 2; k++) {
            const o = ((y * 2 + k) * DOTS + x) * 4;
            const g = k ? L * 0.25 : L;
            d[o] = 255 * P4[0] * g + 6;
            d[o + 1] = 255 * P4[1] * g + 7;
            d[o + 2] = 255 * P4[2] * g + 7;
            d[o + 3] = 255;
          }
        }
      octx.putImageData(img, 0, 0);
    },
    draw() {
      ctx.fillStyle = "#060707";
      ctx.fillRect(0, 0, canvas.width, canvas.height);
      const w = canvas.width * RASTER_ON_GLASS[0];
      const h = canvas.height * RASTER_ON_GLASS[1];
      ctx.imageSmoothingEnabled = true;
      ctx.drawImage(off, (canvas.width - w) / 2, (canvas.height - h) / 2, w, h);
    },
  };
}

let crt;
try {
  crt = makeGL(canvas);
} catch (e) {
  console.warn("WebGL2 CRT unavailable:", e);
}
if (!crt) crt = make2D(canvas);

function fitCanvas() {
  const dpr = Math.min(window.devicePixelRatio || 1, 2);
  const w = Math.round(canvas.clientWidth * dpr);
  const h = Math.round(canvas.clientHeight * dpr);
  if (w && h && (canvas.width !== w || canvas.height !== h)) {
    canvas.width = w;
    canvas.height = h;
    return true;
  }
  return false;
}

// ---------------------------------------------------------------- sound

let audio = null;
let clickBuffer = null;
const soundBox = document.getElementById("sound");

function ensureAudio() {
  if (!audio) {
    const Ctx = window.AudioContext || window.webkitAudioContext;
    if (!Ctx) return;
    audio = new Ctx();
    // "C8 discharges through the speaker ... generating a click"
    const n = Math.round(audio.sampleRate * 0.006);
    clickBuffer = audio.createBuffer(1, n, audio.sampleRate);
    const d = clickBuffer.getChannelData(0);
    for (let i = 0; i < n; i++) {
      const t = i / audio.sampleRate;
      d[i] = Math.exp(-t / 0.0007) * (Math.random() * 0.6 + Math.sin(2 * Math.PI * 2400 * t) * 0.8);
    }
  }
  if (audio.state === "suspended") audio.resume();
}

function playClick() {
  if (!audio || !soundBox.checked) return;
  const src = audio.createBufferSource();
  const gain = audio.createGain();
  gain.gain.value = 0.35;
  src.buffer = clickBuffer;
  src.connect(gain).connect(audio.destination);
  src.start();
}

// "an 800 hertz tone. Bell is generated by setting the bell bit for 0.25 seconds"
function playBell() {
  if (!audio || !soundBox.checked) return;
  const osc = audio.createOscillator();
  const gain = audio.createGain();
  osc.type = "square";
  osc.frequency.value = 800;
  const t = audio.currentTime;
  gain.gain.setValueAtTime(0, t);
  gain.gain.linearRampToValueAtTime(0.06, t + 0.005);
  gain.gain.setValueAtTime(0.06, t + 0.245);
  gain.gain.linearRampToValueAtTime(0, t + 0.25);
  osc.connect(gain).connect(audio.destination);
  osc.start(t);
  osc.stop(t + 0.26);
}

// ---------------------------------------------------------------- keyboard

// Physical keys. Letters follow the character typed, so other layouts get the letters on their
// keycaps; everything else is by position on a US keyboard, which is where the VT100 had it.
const CODES = {
  Enter: VK.RETURN, NumpadEnter: VK.KP_ENTER, Backspace: VK.BACKSPACE, Delete: VK.DELETE,
  Tab: VK.TAB, Escape: VK.ESC, Insert: VK.LINEFEED, Pause: VK.BREAK, ScrollLock: VK.NOSCROLL,
  ArrowUp: VK.UP, ArrowDown: VK.DOWN, ArrowLeft: VK.LEFT, ArrowRight: VK.RIGHT,
  F1: VK.PF1, F2: VK.PF2, F3: VK.PF3, F4: VK.PF4,
  NumpadSubtract: VK.KP_MINUS, NumpadAdd: VK.KP_COMMA, NumpadDecimal: VK.KP_PERIOD,
  ShiftLeft: VK.SHIFT, ShiftRight: VK.SHIFT, ControlLeft: VK.CTRL, ControlRight: VK.CTRL,
  CapsLock: VK.CAPSLOCK, Space: 32,
  Minus: 45, Equal: 61, Backquote: 96, BracketLeft: 91, BracketRight: 93, Semicolon: 59,
  Quote: 39, Comma: 44, Period: 46, Slash: 47, Backslash: 92,
};
for (let i = 0; i < 10; i++) {
  CODES["Digit" + i] = 48 + i;
  CODES["Numpad" + i] = VK.KP0 + i;
}

function vtKeyFor(e) {
  if (/^[a-zA-Z]$/.test(e.key)) return e.key.toLowerCase().charCodeAt(0);
  if (/^Key[A-Z]$/.test(e.code)) return e.code.charCodeAt(3) + 32;
  return CODES[e.code];
}

let therac = null;
const held = new Map(); // physical code -> VT100 key
const keyEls = new Map(); // VT100 key -> on-screen key elements

function sendKey(key, down) {
  if (!therac) return;
  therac.key(key, down, performance.now());
  for (const el of keyEls.get(key) ?? []) el.classList.toggle("down", down);
}

function terminalHasFocus() {
  const a = document.activeElement;
  return a === canvas || a === document.body || a?.classList?.contains("key");
}

document.addEventListener("keydown", (e) => {
  if (!terminalHasFocus() || e.metaKey || e.altKey) return;
  const key = vtKeyFor(e);
  if (key === undefined) return;
  e.preventDefault();
  ensureAudio();
  hideHint();
  if (e.repeat) return; // the VT100 does its own auto repeat
  held.set(e.code, key);
  sendKey(key, true);
});

document.addEventListener("keyup", (e) => {
  const key = held.get(e.code);
  if (key === undefined) return;
  held.delete(e.code);
  e.preventDefault();
  sendKey(key, false);
});

window.addEventListener("blur", () => {
  for (const key of held.values()) sendKey(key, false);
  held.clear();
});

// The drawn keyboard, positioned from DEC's drawing of the VT100 keyboard (972 x 300 units).
// [x, y, width, height, legend, VT100 key, shifted legend]
const LAYOUT = [
  [48, 28, 60, 40, "SET-UP", VK.SETUP],
  [532, 28, 40, 40, "↑", VK.UP], [576, 28, 40, 40, "↓", VK.DOWN],
  [620, 28, 40, 40, "←", VK.LEFT], [664, 28, 40, 40, "→", VK.RIGHT],
  [788, 28, 40, 40, "PF1", VK.PF1], [832, 28, 40, 40, "PF2", VK.PF2],
  [876, 28, 40, 40, "PF3", VK.PF3], [920, 28, 40, 40, "PF4", VK.PF4],
  [48, 72, 40, 40, "ESC", VK.ESC],
  ...[..."1234567890"].map((c, i) => [92 + 44 * i, 72, 40, 40, c, c.charCodeAt(0), "!@#$%^&*()"[i]]),
  [532, 72, 40, 40, "-", 45, "_"], [576, 72, 40, 40, "=", 61, "+"], [620, 72, 40, 40, "`", 96, "~"],
  [664, 72, 40, 40, "BACK SPACE", VK.BACKSPACE], [708, 72, 40, 40, "BREAK", VK.BREAK],
  [788, 72, 40, 40, "7", VK.KP7], [832, 72, 40, 40, "8", VK.KP8],
  [876, 72, 40, 40, "9", VK.KP9], [920, 72, 40, 40, "-", VK.KP_MINUS],
  [48, 116, 56, 40, "TAB", VK.TAB],
  ...[..."QWERTYUIOP"].map((c, i) => [108 + 44 * i, 116, 40, 40, c, c.toLowerCase().charCodeAt(0)]),
  [548, 116, 40, 40, "[", 91, "{"], [592, 116, 40, 40, "]", 93, "}"],
  [616, 116, 60, 84, "RETURN", VK.RETURN, null, "return"],
  [680, 116, 40, 40, "DELETE", VK.DELETE],
  [788, 116, 40, 40, "4", VK.KP4], [832, 116, 40, 40, "5", VK.KP5],
  [876, 116, 40, 40, "6", VK.KP6], [920, 116, 40, 40, ",", VK.KP_COMMA],
  [12, 160, 40, 40, "CTRL", VK.CTRL], [56, 160, 72, 40, "CAPS LOCK", VK.CAPSLOCK],
  ...[..."ASDFGHJKL"].map((c, i) => [132 + 44 * i, 160, 40, 40, c, c.toLowerCase().charCodeAt(0)]),
  [528, 160, 40, 40, ";", 59, ":"], [572, 160, 40, 40, "'", 39, '"'], [680, 160, 40, 40, "\\", 92, "|"],
  [788, 160, 40, 40, "1", VK.KP1], [832, 160, 40, 40, "2", VK.KP2],
  [876, 160, 40, 40, "3", VK.KP3], [920, 160, 40, 84, "ENTER", VK.KP_ENTER],
  [12, 204, 40, 40, "NO SCROLL", VK.NOSCROLL], [56, 204, 94, 40, "SHIFT", VK.SHIFT],
  ...[..."ZXCVBNM"].map((c, i) => [154 + 44 * i, 204, 40, 40, c, c.toLowerCase().charCodeAt(0)]),
  [462, 204, 40, 40, ",", 44, "<"], [506, 204, 40, 40, ".", 46, ">"], [550, 204, 40, 40, "/", 47, "?"],
  [594, 204, 72, 40, "SHIFT", VK.SHIFT], [670, 204, 40, 40, "LINE FEED", VK.LINEFEED],
  [788, 204, 84, 40, "0", VK.KP0], [876, 204, 40, 40, ".", VK.KP_PERIOD],
  [172, 248, 400, 40, "", 32],
];
const LEDS = ["ON LINE", "LOCAL", "KBD LOCKED", "L1", "L2", "L3", "L4"];
const MODIFIERS = new Set([VK.SHIFT, VK.CTRL]);

const pct = (v, of) => `${(v / of) * 100}%`;

function buildKeyboard() {
  const kb = document.getElementById("keyboard");
  const panel = document.createElement("div");
  panel.className = "leds";
  Object.assign(panel.style, { left: pct(112, 972), top: pct(12, 300), width: pct(416, 972), height: pct(56, 300) });
  kb.append(panel);
  const ledEls = LEDS.map((label, i) => {
    const led = document.createElement("div");
    led.className = "led";
    led.innerHTML = `<span>${label.replace(" ", "<br>")}</span><i></i>`;
    Object.assign(led.style, { left: pct(95 + 40 * i, 416), top: "14%" });
    panel.append(led);
    return led;
  });

  // touch: SHIFT and CTRL stay down until the next key is released
  const latched = new Set();
  for (const [x, y, w, h, legend, key, shifted, cls] of LAYOUT) {
    const el = document.createElement("button");
    el.type = "button";
    const kind = legend.length === 1 && !shifted ? " letter" : legend.length > 3 && !legend.includes(" ") ? " word" : "";
    el.className = "key" + kind + (cls ? " " + cls : "");
    el.setAttribute("aria-label", legend || "space");
    el.tabIndex = -1;
    Object.assign(el.style, { left: pct(x, 972), top: pct(y, 300), width: pct(w, 972), height: pct(h, 300) });
    if (shifted) el.innerHTML = `<span class="small">${shifted}</span><span>${legend}</span>`;
    else if (legend.includes(" ")) el.innerHTML = legend.split(" ").map((p) => `<span class="small">${p}</span>`).join("");
    else el.textContent = legend;
    if (!keyEls.has(key)) keyEls.set(key, []);
    keyEls.get(key).push(el);

    el.addEventListener("pointerdown", (e) => {
      e.preventDefault();
      ensureAudio();
      hideHint();
      canvas.focus({ preventScroll: true });
      el.setPointerCapture(e.pointerId);
      if (MODIFIERS.has(key)) {
        if (latched.has(key)) {
          latched.delete(key);
          sendKey(key, false);
        } else {
          latched.add(key);
          sendKey(key, true);
        }
        return;
      }
      sendKey(key, true);
    });
    const release = () => {
      if (MODIFIERS.has(key) || !el.classList.contains("down")) return;
      sendKey(key, false);
      for (const m of latched) sendKey(m, false);
      latched.clear();
    };
    el.addEventListener("pointerup", release);
    el.addEventListener("pointercancel", release);
    kb.append(el);
  }
  return ledEls;
}

const ledEls = buildKeyboard();
let lastLeds = -1;

function showLeds(bits) {
  if (bits === lastLeds) return;
  lastLeds = bits;
  ledEls.forEach((el, i) => el.classList.toggle("on", (bits >> i) & 1));
}

// ---------------------------------------------------------------- the rest of the room

let hintShown = false;
function hideHint() {
  if (hintShown) {
    statusLine.textContent = "";
    hintShown = false;
  }
}

canvas.addEventListener("pointerdown", () => {
  ensureAudio();
  canvas.focus({ preventScroll: true });
});

function refocus() {
  canvas.focus({ preventScroll: true });
}

document.getElementById("field-light").addEventListener("click", () => {
  therac?.hand(HAND_FIELD_LIGHT);
  refocus();
});
document.getElementById("set-button").addEventListener("click", () => {
  therac?.hand(HAND_SET);
  refocus();
});
document.getElementById("power").addEventListener("click", () => {
  therac?.powerCycle();
  refocus();
});
document.getElementById("underline").addEventListener("change", (e) => {
  therac?.cursorStyle(!e.target.checked);
  refocus();
});

const hiddenPanel = document.getElementById("hidden-panel");
const out = (id) => document.getElementById(id);
const TURNTABLE = {
  CollimatorPositionXRay: "X-ray",
  CollimatorPositionElectronBeam: "electron",
  CollimatorPositionFieldLight: "field light",
  CollimatorPositionUndefined: "–",
};

function showHidden() {
  const phase = therac.info(REQUEST.phase).replace(/^TP_/, "");
  const c3 = Number(therac.info(REQUEST.class3));
  const beam = therac.info(REQUEST.hardwareBeam);
  const kev = Number(therac.info(REQUEST.hardwareEnergy));
  out("i-phase").textContent = phase;
  // Ptime only looks for edits while the bending magnet flag is set, during the first magnet
  const magnet = Number(therac.info(REQUEST.magnet));
  const watching = therac.info(REQUEST.bendingMagnetFlag) === "1";
  out("i-magnets").textContent =
    magnet === 0 ? (beam === "BeamTypeUndefined" ? "–" : "set") : `setting ${magnet} of 4: an edit now is ${watching ? "noticed" : "missed"}`;
  out("i-class3").textContent = String(c3).padStart(3, " ");
  out("i-class3-bar").style.width = `${(c3 / 255) * 100}%`;
  out("i-turntable").textContent = TURNTABLE[therac.info(REQUEST.turntable)] ?? "–";
  out("i-beam").textContent =
    beam === "BeamTypeXRay" ? `X-ray, ${kev / 1000} MeV` : beam === "BeamTypeElectron" ? `electrons, ${kev / 1000} MeV` : "–";
  out("i-shown").textContent = `${therac.info(REQUEST.displayedDose)} MU`;
  out("i-dose").textContent = `${therac.info(REQUEST.patientDose)} rads`;
}

// ---------------------------------------------------------------- main loop

let lastHidden = 0;
let lastMirror = 0;
let mirrorText = "";

function frame(now) {
  const resized = fitCanvas();
  if (therac.frame(now)) {
    crt.upload(therac.raster());
    crt.draw();
  } else if (resized) {
    crt.draw();
  }
  for (let n = therac.takeClicks(); n > 0; n--) playClick();
  for (let n = therac.takeBells(); n > 0; n--) playBell();
  showLeds(therac.leds());
  if (hiddenPanel.open && now - lastHidden > 100) {
    lastHidden = now;
    showHidden();
  }
  if (now - lastMirror > 1000) {
    lastMirror = now;
    const text = therac.text().replace(/ +$/gm, "");
    if (text !== mirrorText) mirror.textContent = mirrorText = text;
  }
  requestAnimationFrame(frame);
}

// build-web.sh writes the repository URL here, or leaves it empty
function showSource() {
  const url = document.querySelector('meta[name="therac-source"]')?.content.trim();
  if (!url) return;
  const link = document.getElementById("source-link");
  link.href = url;
  link.textContent = url.replace(/^https?:\/\//, "");
  document.getElementById("source-note").hidden = false;
}

async function start() {
  showSource();
  try {
    therac = await loadTherac(await fetch("therac.wasm"));
  } catch (e) {
    statusLine.textContent = "Could not start the simulator: " + e;
    throw e;
  }
  fitCanvas();
  therac.start(performance.now());
  statusLine.textContent = "Click the screen, then type.";
  hintShown = true;
  canvas.focus({ preventScroll: true });
  // expose for the browser tests and the curious
  window.therac = therac;
  requestAnimationFrame(frame);
}

start();
