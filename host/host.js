// host.js — browser side of the machine.
//
// Loads kernel.wasm, forwards keyboard and mouse events into the kernel's
// event queue, and once per animation frame asks the kernel to run; when the
// kernel reports a changed screen, its RGBA buffer is copied to the canvas.
// It also keeps the saved world (an image of the Lisp heap) in IndexedDB.
//
// Keep EV / MOD / KEY in sync with kernel/kernel.h.
"use strict";

const EV = { MOUSE_MOVE: 1, MOUSE_DOWN: 2, MOUSE_UP: 3, KEY_DOWN: 4, KEY_UP: 5, FOCUS: 6, BLUR: 7, FILES: 10 };
const MOD = { CONTROL: 0x01, META: 0x02, SUPER: 0x04, HYPER: 0x08, SHIFT: 0x10 };
const KEY = {
  Enter: 0x110100, Backspace: 0x110101, Tab: 0x110102, Escape: 0x110103,
  F1: 0x110104, F2: 0x110105, F3: 0x110106, F4: 0x110107,
  ArrowLeft: 0x110108, ArrowRight: 0x110109, ArrowUp: 0x11010a, ArrowDown: 0x11010b,
};

const PUNCT = {
  Period: ".>", Comma: ",<", Slash: "/?", Semicolon: ";:", Quote: "'\"", Minus: "-_",
  Equal: "=+", BracketLeft: "[{", BracketRight: "]}", Backslash: "\\|", Backquote: "`~",
};

// Browser shortcuts we leave alone (reload, dev tools).
const PASS_THROUGH = new Set(["KeyR", "KeyI", "KeyJ"]);

function mods(e) {
  return (e.ctrlKey ? MOD.CONTROL : 0) | (e.altKey ? MOD.META : 0) |
         (e.metaKey ? MOD.SUPER : 0) | (e.shiftKey ? MOD.SHIFT : 0);
}

// Map a KeyboardEvent to a kernel key code, or 0 if we don't handle it.
function keyCode(e) {
  if (e.key in KEY) return KEY[e.key];
  if (e.code === "Space") return 32;
  // With modifiers held (Option on macOS produces ∂, ß, …), use the physical
  // key so that m-X really is X.
  if (e.altKey || e.ctrlKey || e.metaKey) {
    let m = /^Key([A-Z])$/.exec(e.code);
    if (m) return (e.shiftKey ? m[1] : m[1].toLowerCase()).charCodeAt(0);
    m = /^Digit(\d)$/.exec(e.code);
    if (m) return m[1].charCodeAt(0);
    // punctuation, as on a US keyboard: m-. m-< m-> m-; ...
    const p = PUNCT[e.code];
    if (p) return p.charCodeAt(e.shiftKey ? 1 : 0);
  }
  // any character the keyboard types, with dead keys and the like already
  // composed by the browser: å, é, ß, λ ... (M15)
  const c = e.key.codePointAt(0);
  if (e.key.length === (c > 0xffff ? 2 : 1) && (c >= 32 && c < 127 || c >= 160)) return c;
  return 0;
}

// ---- saved worlds (IndexedDB) ------------------------------------------------
//
// The world image is kept in IndexedDB under one key. It is written shortly
// after each top-level form finishes (and on (disk-save)), and loaded at boot.

// Version 2 adds the editor's files ("files": name -> bytes), kept here when
// there is no file server, and "meta" (the folder chosen for files, if any).
const DB_NAME = "lisp-machine", STORE = "worlds", CURRENT = "current";

function openDb() {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open(DB_NAME, 2);
    req.onupgradeneeded = () => {
      for (const name of [STORE, "files", "meta"])
        if (!req.result.objectStoreNames.contains(name)) req.result.createObjectStore(name);
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

async function dbRequest(mode, makeRequest, store = STORE) {
  const db = await openDb();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(store, mode);
    const req = makeRequest(tx.objectStore(store));
    tx.oncomplete = () => { db.close(); resolve(req.result); };
    tx.onerror = () => { db.close(); reject(tx.error); };
  });
}

const readWorld = () => dbRequest("readonly", (s) => s.get(CURRENT));
const writeWorld = (record) => dbRequest("readwrite", (s) => s.put(record, CURRENT));
const deleteWorld = () => dbRequest("readwrite", (s) => s.delete(CURRENT));

const kb = (n) => Math.round(n / 1024);

// the order is the kernel's (kernel/float.c)
const MATH_OPS = [Math.exp, Math.log, Math.sin, Math.cos, Math.tan, Math.asin, Math.acos,
                  Math.atan2, Math.pow, Math.sinh, Math.cosh, Math.tanh];

// lisp.html carries the kernel inside it (tools/single.py), as base64
async function loadKernel(imports) {
  if (typeof KERNEL_WASM === "string") {
    const bytes = Uint8Array.from(atob(KERNEL_WASM), (c) => c.charCodeAt(0));
    return (await WebAssembly.instantiate(bytes, imports)).instance;
  }
  const url = "kernel.wasm";
  if (WebAssembly.instantiateStreaming) {
    try {
      return (await WebAssembly.instantiateStreaming(fetch(url), imports)).instance;
    } catch (err) {
      console.warn("instantiateStreaming failed, retrying:", err);
    }
  }
  const bytes = await (await fetch(url)).arrayBuffer();
  return (await WebAssembly.instantiate(bytes, imports)).instance;
}

async function boot() {
  const canvas = document.getElementById("screen");
  const status = document.getElementById("status");
  const ctx = canvas.getContext("2d", { alpha: false });

  let memory = null;
  const decoder = new TextDecoder();
  const imports = {
    env: {
      host_log(ptr, len) {
        console.log(decoder.decode(new Uint8Array(memory.buffer, ptr, len)));
      },
      // floats (kernel/float.c): what a libm would do natively
      host_math(op, x, y) {
        return MATH_OPS[op] ? MATH_OPS[op](x, y) : NaN;
      },
      // the shortest digits that read back as x, as "d.ddde+x"
      host_float_digits(x, ptr, cap) {
        const s = x.toExponential();
        const out = new Uint8Array(memory.buffer, ptr, cap);
        const n = Math.min(s.length, cap);
        for (let i = 0; i < n; i++) out[i] = s.charCodeAt(i);
        return n;
      },
      host_float_parse(ptr, len) {
        return Number(decoder.decode(new Uint8Array(memory.buffer, ptr, len)));
      },
      // the wall clock and the local time zone (M17)
      host_wall_clock() { return Date.now(); },
      host_utc_offset(ms) { return -new Date(ms).getTimezoneOffset(); },
    },
  };

  const instance = await loadKernel(imports);
  const k = instance.exports;
  memory = k.memory;

  // Boot from the saved world if there is one (unless the URL says ?fresh).
  let saved = null, restored = false;
  if (!new URLSearchParams(location.search).has("fresh")) {
    try { saved = await readWorld(); } catch (e) { console.warn("no saved world:", e); }
  }
  if (saved && saved.bytes) {
    const bytes = new Uint8Array(saved.bytes);
    const ptr = k.kernel_image_buffer(bytes.length);
    if (ptr) {
      new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
      restored = !!k.kernel_init_from_image(bytes.length);
    } else {
      k.kernel_init();
    }
  } else {
    k.kernel_init();
  }

  // ---- screen size and zoom -----------------------------------------------
  //
  // The zoom is a whole number of *device* pixels per screen pixel, so every
  // pixel of the machine is a crisp block. The machine's screen is the window
  // divided by the zoom: a bigger zoom gives bigger text and a smaller screen.
  //
  // We do the pixel replication ourselves: the kernel's W x H image goes to an
  // offscreen canvas, which is drawn onto the visible canvas at exactly
  // W*zoom x H*zoom device pixels with smoothing off. The browser then shows
  // the visible canvas 1:1, so it never resamples it. That way it does not
  // matter whether a browser honours CSS image-rendering (Safari has been
  // inconsistent about it).

  const back = document.createElement("canvas");
  const backCtx = back.getContext("2d", { alpha: false });

  const ZOOM_KEY = "lisp.zoom";
  const dpr = () => window.devicePixelRatio || 1;
  const defaultZoom = () => Math.max(1, Math.round(2 * dpr()));   // 2 CSS px per pixel
  let zoom = defaultZoom();
  try {
    const saved = parseInt(localStorage.getItem(ZOOM_KEY), 10);
    if (saved >= 1 && saved <= 12) zoom = saved;
  } catch (e) { /* storage unavailable: use the default */ }

  let W = 0, H = 0, image = null;

  function layout() {
    const want_w = Math.floor(innerWidth * dpr() / zoom);
    const want_h = Math.floor(innerHeight * dpr() / zoom);
    k.kernel_resize(want_w, want_h);           // the kernel clamps to its limits
    const w = k.kernel_screen_width(), h = k.kernel_screen_height();
    if (w !== W || h !== H) {
      W = w; H = h;
      back.width = W;
      back.height = H;
      image = null;
    }
    if (canvas.width !== W * zoom || canvas.height !== H * zoom) {
      canvas.width = W * zoom;         // device pixels
      canvas.height = H * zoom;
    }
    canvas.style.width = (W * zoom / dpr()) + "px";
    canvas.style.height = (H * zoom / dpr()) + "px";
    needsPresent = true;
  }
  let needsPresent = true;

  // devicePixelRatio changes when the window moves to another display
  function watchDpr() {
    const mq = matchMedia(`(resolution: ${dpr()}dppx)`);
    mq.addEventListener("change", () => { layout(); watchDpr(); }, { once: true });
  }
  watchDpr();

  let statusTimer = 0;
  function showStatus(text) {
    status.textContent = text;
    status.style.opacity = "0.8";
    clearTimeout(statusTimer);
    statusTimer = setTimeout(() => { status.style.opacity = "0"; }, 1500);
  }

  function setZoom(z) {
    zoom = Math.min(12, Math.max(1, z));
    try { localStorage.setItem(ZOOM_KEY, String(zoom)); } catch (e) { /* ignore */ }
    layout();
    showStatus(`zoom ${+(zoom / dpr()).toFixed(2)}x  -  screen ${W} x ${H}`);
  }

  addEventListener("resize", layout);
  layout();

  function present() {
    if (!image || image.data.buffer !== memory.buffer) {
      const ptr = k.kernel_present_buffer();
      image = new ImageData(new Uint8ClampedArray(memory.buffer, ptr, W * H * 4), W, H);
    }
    backCtx.putImageData(image, 0, 0);
    ctx.imageSmoothingEnabled = false;   // reset whenever the canvas is resized
    ctx.drawImage(back, 0, 0, W * zoom, H * zoom);
    needsPresent = false;
  }

  // ---- input -------------------------------------------------------------

  const post = (type, a, b, m) => k.kernel_post_event(type, a | 0, b | 0, m | 0);

  function mousePos(e) {
    const r = canvas.getBoundingClientRect();
    return [Math.floor((e.clientX - r.left) * W / r.width),
            Math.floor((e.clientY - r.top) * H / r.height)];
  }

  let buttons = 0;
  addEventListener("mousemove", (e) => {
    const [x, y] = mousePos(e);
    post(EV.MOUSE_MOVE, x, y, mods(e));
  });
  canvas.addEventListener("mousedown", (e) => {
    e.preventDefault();
    canvas.focus();
    const [x, y] = mousePos(e);
    post(EV.MOUSE_MOVE, x, y, mods(e));
    buttons |= 1 << e.button;
    post(EV.MOUSE_DOWN, e.button, 0, mods(e));
  });
  addEventListener("mouseup", (e) => {
    if (!(buttons & (1 << e.button))) return;
    buttons &= ~(1 << e.button);
    post(EV.MOUSE_UP, e.button, 0, mods(e));
  });
  canvas.addEventListener("contextmenu", (e) => e.preventDefault());

  // ---- saving the world ----------------------------------------------------

  let autosave = true;          // off after (forget-world), until (disk-save)
  let saving = false, wantSave = false;
  let savedGen = k.kernel_world_generation(), lastGen = savedGen, changedAt = 0;

  // A copy of the world image, or null while a computation runs.
  function capture() {
    const n = k.kernel_image_capture();
    if (n <= 0) return null;
    const ptr = k.kernel_image_data();
    return memory.buffer.slice(ptr, ptr + n);
  }

  async function saveWorld(explicit) {
    if (saving) return;
    const bytes = capture();
    if (!bytes) return;
    saving = true;
    savedGen = k.kernel_world_generation();
    try {
      await writeWorld({ bytes, saved: Date.now(), size: bytes.byteLength });
      if (explicit) showStatus(`world saved (${kb(bytes.byteLength)} KB)`);
    } catch (e) {
      showStatus("could not save the world: " + e.message);
    } finally {
      saving = false;
    }
  }

  function worldHousekeeping(t) {
    const req = k.kernel_world_requests();
    if (req & 2) {
      autosave = false;
      deleteWorld().then(() => showStatus("saved world forgotten: the next start is fresh"),
                         (e) => showStatus("could not forget the world: " + e.message));
    }
    if (req & 1) wantSave = true;
    const gen = k.kernel_world_generation();
    if (gen !== lastGen) { lastGen = gen; changedAt = t; }
    if (saving || k.kernel_busy()) return;
    if (wantSave) { wantSave = false; autosave = true; saveWorld(true); }
    else if (autosave && gen !== savedGen && t - changedAt > 1500) saveWorld(false);
  }

  // leaving or hiding the page: save now if anything changed (best effort)
  const saveIfChanged = () => {
    if (autosave && k.kernel_world_generation() !== savedGen && !k.kernel_busy()) saveWorld(false);
  };
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") saveIfChanged();
  });
  addEventListener("pagehide", saveIfChanged);

  // Cmd+S: download the world as a file
  function downloadWorld() {
    const bytes = capture();
    if (!bytes) { showStatus("busy: save when the computation has finished"); return; }
    const a = document.createElement("a");
    a.href = URL.createObjectURL(new Blob([bytes], { type: "application/octet-stream" }));
    a.download = `lisp-${new Date().toISOString().slice(0, 10)}.world`;
    a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
    showStatus(`world downloaded (${kb(bytes.byteLength)} KB)`);
  }

  // dropping a .world file on the page makes it the saved world and restarts
  addEventListener("dragover", (e) => e.preventDefault());
  addEventListener("drop", async (e) => {
    e.preventDefault();
    const lisp = await readFiles(e.dataTransfer.files);
    if (lisp.length) {                            // .lisp files: to the editor (file op 8)
      dropped.push(...lisp);
      post(EV.FILES, lisp.length, 0, 0);
      showStatus(`${lisp.map((f) => f.name).join(", ")}: in the editor`);
      return;
    }
    const file = e.dataTransfer.files[0];
    if (!file) return;
    try {
      const bytes = await file.arrayBuffer();
      await writeWorld({ bytes, saved: Date.now(), size: bytes.byteLength, from: file.name });
      showStatus(`loading ${file.name}...`);
      location.href = location.pathname;   // reload, without ?fresh
    } catch (err) {
      showStatus("could not load " + file.name + ": " + err.message);
    }
  });

  addEventListener("keydown", (e) => {
    if (e.target.closest && e.target.closest(".ask")) return;   // the keys belong to the dialog
    if (e.metaKey && PASS_THROUGH.has(e.code)) return;
    // Cmd-S (or Ctrl-Shift-S) downloads the world; plain c-S is the editor's search
    if (((e.metaKey && !e.ctrlKey) || (e.ctrlKey && e.shiftKey)) && !e.altKey && e.code === "KeyS") {
      e.preventDefault();
      downloadWorld();
      return;
    }
    // Cmd/Ctrl + = - 0: zoom the machine's screen, not the page
    if ((e.metaKey || e.ctrlKey) && !e.altKey) {
      if (e.key === "=" || e.key === "+") { e.preventDefault(); setZoom(zoom + 1); return; }
      if (e.key === "-" || e.key === "_") { e.preventDefault(); setZoom(zoom - 1); return; }
      if (e.key === "0") { e.preventDefault(); setZoom(defaultZoom()); return; }
    }
    const code = keyCode(e);
    if (!code) return;
    e.preventDefault();
    post(EV.KEY_DOWN, code, 0, mods(e));
  });
  addEventListener("keyup", (e) => {
    if (e.target.closest && e.target.closest(".ask")) return;
    const code = keyCode(e);
    if (code) post(EV.KEY_UP, code, 0, mods(e));
  });
  addEventListener("focus", () => post(EV.FOCUS, 0, 0, 0));
  addEventListener("blur", () => { buttons = 0; post(EV.BLUR, 0, 0, 0); });

  // ---- the world's source files (the editor, world/editor.lisp) ----------
  //
  // The kernel holds one request at a time (kernel/kernel.c); we take it after
  // a frame and answer with kernel_file_reply. The files are kept, best first:
  //   1 on the development server (make serve, tools/serve.py): world/ on disk
  //   3 in a folder the user chose (Chrome, Edge: the File System Access API)
  //   2 in the browser's storage (IndexedDB), next to the saved world
  // Bytes pass through unchanged. Uploads and dropped files go to the editor's
  // buffers, not to storage: the user saves them where they belong.

  const FILES_SERVER = 1, FILES_BROWSER = 2, FILES_FOLDER = 3;
  let fileKind = FILES_BROWSER, folder = null, dropped = [];
  const encoder = new TextEncoder();
  const fileDb = (mode, f) => dbRequest(mode, f, "files");
  const metaDb = (mode, f) => dbRequest(mode, f, "meta");

  async function initFiles() {
    try {
      const r = await fetch("files/", { cache: "no-store" });
      const text = r.ok ? await r.text() : "";
      if (r.ok && /\.lisp\n/.test(text)) fileKind = FILES_SERVER;
    } catch (e) { /* no server: opened from a file, or a plain web host */ }
    if (fileKind !== FILES_SERVER) {
      try {
        const handle = await metaDb("readonly", (s) => s.get("folder"));
        if (handle && (await handle.queryPermission({ mode: "readwrite" })) === "granted") {
          folder = handle;
          fileKind = FILES_FOLDER;
        }
      } catch (e) { /* no folder chosen, or the browser has no folders */ }
    }
    k.kernel_set_file_server(fileKind);
  }
  initFiles();

  // A question that needs a click (the browser opens file and folder pickers
  // only from one). Resolves with the result of DOIT, or rejects on Cancel.
  function ask(message, label, doit) {
    return new Promise((resolve, reject) => {
      const box = document.createElement("div");
      box.className = "ask";
      const text = document.createElement("span");
      text.textContent = message;
      const go = document.createElement("button");
      go.textContent = label;
      const cancel = document.createElement("button");
      cancel.textContent = "Cancel";
      const close = () => { box.remove(); canvas.focus(); };
      go.onclick = () => { close(); doit().then(resolve, reject); };
      cancel.onclick = () => { close(); reject(new Error("cancelled")); };
      box.append(text, go, cancel);
      document.body.append(box);
      go.focus();
    });
  }

  // "name\nlength\nbytes" for each file: how the editor receives several
  function packFiles(files) {
    const parts = [];
    for (const f of files) parts.push(encoder.encode(`${f.name}\n${f.bytes.length}\n`), f.bytes);
    const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
    let i = 0;
    for (const p of parts) { out.set(p, i); i += p.length; }
    return out;
  }

  const readFiles = async (list) =>
    Promise.all([...list].filter((f) => f.name.endsWith(".lisp"))
                .map(async (f) => ({ name: f.name, bytes: new Uint8Array(await f.arrayBuffer()) })));

  function pickFiles() {
    return new Promise((resolve) => {
      const input = document.createElement("input");
      input.type = "file";
      input.multiple = true;
      input.accept = ".lisp";
      input.onchange = () => resolve(readFiles(input.files));
      input.oncancel = () => resolve([]);
      input.click();
    });
  }

  function download(name, bytes) {
    const a = document.createElement("a");
    a.href = URL.createObjectURL(new Blob([bytes], { type: "text/plain" }));
    a.download = name;
    a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }

  async function folderReady() {
    if ((await folder.queryPermission({ mode: "readwrite" })) === "granted") return;
    await ask(`Let the machine use the folder "${folder.name}" again?`, "Allow",
              async () => { if ((await folder.requestPermission({ mode: "readwrite" })) !== "granted") throw new Error("not allowed"); });
  }

  const where = () => fileKind === FILES_SERVER ? "world/ on the file server"
                    : fileKind === FILES_FOLDER ? `the folder ${folder.name}` : "the browser";

  // one request => the reply's bytes; an Error for a failure
  async function doFile(op, name, text) {
    const bad = (m) => { throw new Error(m); };
    switch (op) {
    case 1:                                                  // list
      if (fileKind === FILES_SERVER) {
        const r = await fetch("files/", { cache: "no-store" });
        return r.ok ? new Uint8Array(await r.arrayBuffer()) : bad("the server would not list the files");
      }
      if (fileKind === FILES_FOLDER) {
        await folderReady();
        const names = [];
        for await (const [n, h] of folder.entries()) if (h.kind === "file" && n.endsWith(".lisp")) names.push(n);
        return encoder.encode(names.map((n) => n + "\n").join(""));
      }
      return encoder.encode((await fileDb("readonly", (s) => s.getAllKeys())).map((n) => n + "\n").join(""));
    case 2:                                                  // read
      if (fileKind === FILES_SERVER) {
        const r = await fetch("files/" + encodeURIComponent(name), { cache: "no-store" });
        return r.ok ? new Uint8Array(await r.arrayBuffer()) : bad(await r.text());
      }
      if (fileKind === FILES_FOLDER) {
        await folderReady();
        return new Uint8Array(await (await (await folder.getFileHandle(name)).getFile()).arrayBuffer());
      }
      return (await fileDb("readonly", (s) => s.get(name))) || bad("it has not been saved in the browser");
    case 3:                                                  // write
      if (fileKind === FILES_SERVER) {
        const r = await fetch("files/" + encodeURIComponent(name), { method: "PUT", body: text, cache: "no-store" });
        return r.ok ? encoder.encode("ok") : bad(await r.text());
      }
      if (fileKind === FILES_FOLDER) {
        await folderReady();
        const w = await (await folder.getFileHandle(name, { create: true })).createWritable();
        await w.write(text);
        await w.close();
        return encoder.encode("ok");
      }
      await fileDb("readwrite", (s) => s.put(text, name));
      return encoder.encode("ok");
    case 4:                                                  // download
      download(name, text);
      showStatus(`downloaded ${name}`);
      return encoder.encode("ok");
    case 5:                                                  // upload: the user picks files
      return packFiles(await ask("Choose .lisp files to load into the editor", "Choose files…", pickFiles));
    case 6:                                                  // where are the files kept?
      return encoder.encode(where());
    case 7:                                                  // keep them in a folder from now on
      if (!window.showDirectoryPicker) bad("this browser cannot write to a folder: use Download File, or make serve");
      folder = await ask("Choose the folder for the world's files (your world/ folder)", "Choose folder…",
                         () => window.showDirectoryPicker({ mode: "readwrite" }));
      await metaDb("readwrite", (s) => s.put(folder, "folder"));
      fileKind = FILES_FOLDER;
      k.kernel_set_file_server(fileKind);
      return encoder.encode(where());
    case 8: {                                                // the files dropped on the page
      const files = dropped;
      dropped = [];
      return packFiles(files);
    }
    }
    return bad("unknown file operation");
  }

  async function fileRequest(op) {
    const id = k.kernel_file_id();
    const name = decoder.decode(new Uint8Array(memory.buffer, k.kernel_file_name(), k.kernel_file_name_length()));
    const text = new Uint8Array(memory.buffer, k.kernel_file_text(), k.kernel_file_text_length()).slice();
    let status = 0, body;
    try {
      body = await doFile(op, name, text);
    } catch (err) {
      status = 1;
      body = encoder.encode(err.message || String(err));
    }
    const ptr = k.kernel_file_reply_buffer(body.length);
    if (!ptr) { status = 1; body = new Uint8Array(0); }
    else new Uint8Array(memory.buffer, ptr, body.length).set(body);
    k.kernel_file_reply(id, status, ptr ? body.length : 0);
  }

  // ---- frame loop --------------------------------------------------------

  function frame(t) {
    if (k.kernel_frame(t) || needsPresent) present();
    const op = k.kernel_file_request();
    if (op) fileRequest(op);
    worldHousekeeping(t);
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);

  canvas.focus();
  if (restored)
    showStatus(`world restored (${kb(saved.size)} KB, saved ${new Date(saved.saved).toLocaleString()})`);
  else if (saved)
    showStatus("the saved world could not be used: booted from source");
  else
    showStatus(`screen ${W} x ${H}  -  zoom with Cmd +  Cmd -  Cmd 0`);
}

boot().catch((err) => {
  document.getElementById("status").textContent = "boot failed: " + err;
  console.error(err);
});
