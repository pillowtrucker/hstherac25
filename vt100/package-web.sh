#!/bin/sh
# Builds the browser console and packs it as therac25-console.zip: a folder of static files to
# host anywhere, which names neither this repository nor its owner.
#
# - The package and its main module are renamed for this build (they otherwise show up inside
#   the WebAssembly module and its JavaScript glue), and the cabal author/maintainer are blanked.
# - The page links to a repository only if SOURCE_URL is set.
# - Like every build (build-web.sh), it carries LICENSE.txt (AGPL-3.0) and the source it was
#   built from, source.tar.gz, here renamed the same way.
# - The build fails if a name pointing back here is left anywhere in it.
#
# Needs what build-web.sh needs, plus zip. Output: vt100/package/therac25-console.zip
# (OUT to put it elsewhere).
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")
out=${OUT:-$here/package}
name=therac25-console
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
export SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(git -C "$root" log -1 --format=%ct 2>/dev/null || date +%s)}"

# the sources, renamed
src=$stage/src
mkdir -p "$src/vt100"
cd "$root"
cp -r src-lib csrc test LICENSE CHANGELOG.md "$src/"
cp -r vt100/src vt100/hs vt100/test vt100/tools vt100/web vt100/build-web.sh "$src/vt100/"
rm -rf "$src/vt100/web/node_modules"
mv "$src/src-lib/HsTherac25.hs" "$src/src-lib/Therac25.hs"
sed -e 's/^author:.*/author:             -/' -e 's/^maintainer:.*/maintainer:         -/' \
  hstherac25.cabal > "$src/therac25.cabal"
printf 'packages: .\n' > "$src/cabal.project"
find "$src" -type f \( -name '*.hs' -o -name '*.c' -o -name '*.h' -o -name '*.cabal' -o -name '*.md' \) \
  -exec sed -i -e 's/HsTherac25/Therac25/g' -e 's/hstherac25/therac25/g' {} +

check() {
  if grep -r -a -i -l -E 'pillowtrucker|hstherac25|jerkson|janitor' "$@"; then
    echo "package-web.sh: the files above still name the repository or its owner" >&2
    exit 1
  fi
}
check "$src"

# build it
BUILDDIR="$stage/build" "$src/vt100/build-web.sh"
dist=$src/vt100/dist
check "$dist"

cat > "$dist/README.txt" <<'EOF'
Therac-25 treatment console

A DEC VT100 on a 9600-baud line to the PDP-11/23, with a simulation of the treatment software
described in N. G. Leveson and C. S. Turner, "An Investigation of the Therac-25 Accidents",
IEEE Computer 26(7), July 1993. Both software races from the paper can be reproduced; the page
explains how.

Serve this folder with any static web server and open index.html over HTTP, for example
`python3 -m http.server` in this folder. It does not work from a file:// URL.

The simulator and the console are free software under the GNU Affero General Public License,
version 3 or later (LICENSE.txt). source.tar.gz is the source they were built from
(vt100/build-web.sh, with GHC's WebAssembly toolchain); the page links to it, and anyone hosting
this must keep it available. vendor/browser_wasi_shim is MIT or Apache-2.0, see the licence
files there. The terminal's characters are the contents
of DEC's VT100 character-generator ROM, part 23-018E2.
EOF

# the zip: one folder, files in a fixed order with the commit's timestamp
rm -rf "${out:?}/$name" "$out/$name.zip"
mkdir -p "$out"
cp -r "$dist" "$out/$name"
cd "$out"
find "$name" -exec touch -h -d "@$SOURCE_DATE_EPOCH" {} +
find "$name" | LC_ALL=C sort | zip -X -q -@ "$name.zip"
echo "packed $out/$name.zip"
