// Runs the browser build in node and reproduces the Tyler accident by typing on the VT100
// keyboard in real time: node vt100/test/smoke.mjs [vt100/dist]
// It checks what only the wasm build can get wrong: that the simulator's threads run on the
// event loop (threadDelay via setTimeout) and that the C code can call into Haskell.
import { readFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const dist = path.resolve(process.argv[2] ?? path.join(path.dirname(new URL(import.meta.url).pathname), "../dist"));
const { loadTherac, VK, REQUEST } = await import(pathToFileURL(path.join(dist, "therac.mjs")));

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const t = await loadTherac(await readFile(path.join(dist, "therac.wasm")));
t.start(performance.now());
const ticker = setInterval(() => t.frame(performance.now()), 10);

async function press(key) {
  t.key(key, true, performance.now());
  await sleep(20);
  t.key(key, false, performance.now());
  await sleep(20);
}

async function type(s) {
  for (const ch of s) {
    if (ch === "\r") await press(VK.RETURN);
    else await press(ch.toLowerCase().charCodeAt(0));
  }
}

async function waitFor(what, ms) {
  const end = Date.now() + ms;
  while (Date.now() < end) {
    if (t.text().includes(what)) return;
    await sleep(50);
  }
  throw new Error(`timed out waiting for ${what}\n${t.text()}`);
}

try {
  await waitFor("COMMAND:", 5000);
  await type("TEST\r\r");
  const entered = Date.now();
  await type("x\r");
  await type("\r200\r202\r1\r\r\r\r\r\r\r");
  while (Date.now() < entered + 3000) await sleep(20);
  for (let i = 0; i < 11; i++) await press(VK.UP);
  await type("e\r\r\r\r\r\r\r\r\r\r\r");
  await waitFor("BEAM READY", 15000);
  await type("B\r");
  await waitFor("MALFUNCTION 54", 3000);
  const rads = Number(t.info(REQUEST.patientDose));
  console.log(t.text());
  if (!(rads >= 16500)) throw new Error(`expected a Tyler overdose, patient dose ${rads}`);
  console.log(`ok: MALFUNCTION 54 in the browser build, patient received ${rads} rads`);
  clearInterval(ticker);
  process.exit(0);
} catch (e) {
  console.error(String(e));
  process.exit(1);
}
