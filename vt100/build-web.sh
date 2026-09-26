#!/bin/sh
# Builds the browser version of the VT100 console into vt100/dist/.
# Needs GHC's WebAssembly toolchain (https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta,
# FLAVOUR=9.12; `source ~/.ghc-wasm/env`) and npm.
# Serve vt100/dist/ with any static web server, e.g. `python3 -m http.server -d vt100/dist`.
# SOURCE_URL: repository URL for the page's footer link; unset, the page names no repository.
# The page always carries its own source (source.tar.gz) and LICENSE.txt: it is AGPL-3.0.
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
shim=$here/web/node_modules/@bjorn3/browser_wasi_shim
cp -r "$shim/dist" "$dist/vendor/browser_wasi_shim"
cp "$shim/LICENSE-MIT" "$shim/LICENSE-APACHE" "$dist/vendor/browser_wasi_shim/"
cp "$root/LICENSE" "$dist/LICENSE.txt"
# the source this page was built from, as the AGPL requires
epoch=${SOURCE_DATE_EPOCH:-$(git -C "$root" log -1 --format=%ct 2>/dev/null || date +%s)}
sources=
for f in "$root"/*.cabal "$root"/cabal.project "$root"/LICENSE "$root"/README.md "$root"/CHANGELOG.md \
  "$root"/src-lib "$root"/csrc "$root"/test "$root"/vt100; do
  if [ -e "$f" ]; then sources="$sources ${f#"$root"/}"; fi
done
(cd "$root" && tar --sort=name --mtime="@$epoch" --owner=0 --group=0 --numeric-owner \
  --exclude=vt100/dist --exclude=vt100/package --exclude=node_modules \
  --transform 's,^\(\./\)\?,therac25-source/,' -czf "$dist/source.tar.gz" $sources)
cp "$here/web/style.css" "$here/web/main.js" "$here/web/therac.mjs" "$here/web/docs.css" "$dist/"
# "How it works": the walkthrough and the annotated source
python3 "$here/tools/mksource.py" "$root" "$dist"
url=$(printf '%s' "${SOURCE_URL:-}" | sed 's/[&|"<>]//g')
sed "s|<meta name=\"therac-source\" content=\"\">|<meta name=\"therac-source\" content=\"$url\">|" \
  "$here/web/index.html" > "$dist/index.html"
echo "built $dist"
