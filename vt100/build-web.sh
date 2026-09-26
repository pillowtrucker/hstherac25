#!/bin/sh
# Builds the browser version of the VT100 console into vt100/dist/.
# Needs GHC's WebAssembly toolchain (https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta,
# FLAVOUR=9.12; `source ~/.ghc-wasm/env`) and npm.
# Serve vt100/dist/ with any static web server, e.g. `python3 -m http.server -d vt100/dist`.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")
dist=$here/dist
builddir=${BUILDDIR:-$root/dist-newstyle-wasm}

cd "$root"
wasm32-wasi-cabal build --builddir="$builddir" exe:therac-vt100-web
wasm=$(wasm32-wasi-cabal list-bin --builddir="$builddir" exe:therac-vt100-web)

rm -rf "$dist"
mkdir -p "$dist/vendor"
# JavaScript glue for the module's JSFFI imports
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$dist/ghc_wasm_jsffi.js"
if command -v wasm-opt >/dev/null 2>&1; then
  wasm-opt --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext \
    --enable-mutable-globals --enable-reference-types --enable-multivalue --enable-simd \
    -O2 --strip-debug "$wasm" -o "$dist/therac.wasm"
else
  cp "$wasm" "$dist/therac.wasm"
fi

(cd "$here/web" && npm ci --no-audit --no-fund --silent)
cp -r "$here/web/node_modules/@bjorn3/browser_wasi_shim/dist" "$dist/vendor/browser_wasi_shim"
cp "$here/web/index.html" "$here/web/style.css" "$here/web/main.js" "$here/web/therac.mjs" "$dist/"
echo "built $dist"
