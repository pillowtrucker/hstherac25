#!/usr/bin/env python3
"""Builds the site's "How it works" section: the source code as annotated, cross-linked pages.

    python3 vt100/tools/mksource.py ROOT OUT

ROOT is the source tree (the repository, or the renamed copy package-web.sh builds from).
Writes OUT/source/<path>.html for every file in GROUPS, and OUT/learn.html from the
walkthrough in vt100/web/learn.html, where these placeholders are filled in:

    {{sim:NAME}}          link to a top-level definition in the simulator (src-lib/*.hs)
    {{src:PATH#NAME}}     link to a definition, or a whole file without #NAME
    {{assumptions}}       every comment in the code that says ASSUMPTION, linked
    {{files}}             the list of source pages

Comments are set apart from code; inside them, text in "double quotes" (quoted from Leveson
and Turner, 1993) and the word ASSUMPTION are highlighted.
"""
import glob
import html
import os
import re
import sys

GROUPS = [
    ("The Therac-25 software", "the simulator: Treat and its phases, the keyboard handler, the housekeeper",
     ["src-lib/*.hs"]),
    ("Its interface", "what every front end links against", ["csrc/Therac.h", "csrc/Therac.c"]),
    ("The console program", "the operator's screen and keyboard on the PDP-11",
     ["vt100/src/console.h", "vt100/src/console.c"]),
    ("The VT100", "the terminal, from DEC's manuals",
     ["vt100/src/vt100.h", "vt100/src/vt100.c", "vt100/src/vt100_font.inc"]),
    ("The wiring", "the serial line, and how the pieces are put together",
     ["vt100/src/serial.h", "vt100/src/serial.c", "vt100/src/session.h", "vt100/src/session.c",
      "vt100/src/web_main.c", "vt100/hs/WebMain.hs", "vt100/src/native_tty.c", "vt100/hs/NativeMain.hs"]),
    ("The tests", "both accidents, step by step", ["test/Main.hs", "vt100/test/tests.c"]),
]

HS_KEYWORDS = set("""module import qualified as hiding where data type newtype class instance deriving
case of let in if then else do foreign export forall""".split())
C_KEYWORDS = set("""auto break case char const continue default do double else enum extern float for
goto if inline int long register return short signed sizeof static struct switch typedef union
unsigned void volatile while bool true false size_t uint8_t uint16_t int64_t double""".split())

HS_TOKENS = re.compile(r"""
  (?P<pragma>\{-\#.*?\#-\})
| (?P<comment>\{-.*?-\}|--(?![!#$%&*+./<=>?@\\^|~:])[^\n]*)
| (?P<string>"(?:\\.|[^"\\\n])*")
| (?P<char>'(?:\\.|[^'\\\n])')
| (?P<word>[A-Za-z_][A-Za-z0-9_']*)
| (?P<number>\b\d+(?:\.\d+)?\b)
| (?P<other>\s+|.)
""", re.S | re.X)

C_TOKENS = re.compile(r"""
  (?P<comment>/\*.*?\*/|//[^\n]*)
| (?P<pre>^[ \t]*\#[^\n]*)
| (?P<string>"(?:\\.|[^"\\\n])*")
| (?P<char>'(?:\\.|[^'\\\n])+')
| (?P<word>[A-Za-z_][A-Za-z0-9_]*)
| (?P<number>\b(?:0x[0-9A-Fa-f]+|\d+(?:\.\d+)?)\b)
| (?P<other>\s+|.)
""", re.S | re.X | re.M)


def tokens(text, lang):
    rx = HS_TOKENS if lang == "hs" else C_TOKENS
    keywords = HS_KEYWORDS if lang == "hs" else C_KEYWORDS
    for m in rx.finditer(text):
        kind = m.lastgroup
        tok = m.group()
        if kind == "word":
            if tok in keywords:
                kind = "keyword"
            elif lang == "hs" and tok[0].isupper():
                kind = "type"
            else:
                kind = "plain"
        elif kind in ("other", "char"):
            kind = "plain" if kind == "other" else "string"
        yield kind, tok


def comment_html(text):
    """Escapes a comment, marking quotations from the paper and ASSUMPTION."""
    out = []
    for part in re.split(r'("[^"\n]{3,}"|ASSUMPTION)', text):
        if part == "ASSUMPTION":
            out.append('<strong class="assume">ASSUMPTION</strong>')
        elif part.startswith('"') and part.endswith('"') and len(part) > 2:
            out.append('<span class="quote">%s</span>' % html.escape(part, quote=False))
        else:
            out.append(html.escape(part, quote=False))
    return "".join(out)


def highlight(text, lang):
    """Returns one HTML string per source line."""
    lines = [""]
    for kind, tok in tokens(text, lang):
        for i, piece in enumerate(tok.split("\n")):
            if i:
                lines.append("")
            if not piece:
                continue
            if kind == "plain":
                lines[-1] += html.escape(piece, quote=False)
            elif kind == "comment":
                lines[-1] += '<span class="c">%s</span>' % comment_html(piece)
            else:
                cls = {"keyword": "k", "type": "t", "string": "s", "number": "n",
                       "pragma": "p", "pre": "p"}[kind]
                lines[-1] += '<span class="%s">%s</span>' % (cls, html.escape(piece, quote=False))
    if lines and lines[-1] == "":
        lines.pop()
    return lines


HS_DEF = re.compile(r"^(?:data|newtype|type)\s+([A-Z][A-Za-z0-9_']*)|^([a-z_][A-Za-z0-9_']*)\s*::")
C_DEF = re.compile(r"^(?:static\s+)?(?:const\s+)?[A-Za-z_][A-Za-z0-9_ ]*[ \*]+\**([a-z_][A-Za-z0-9_]*)\s*\([^;]*$"
                   r"|^typedef\s+(?:struct|enum)\s+([A-Za-z_][A-Za-z0-9_]*)"
                   r"|^(?:struct|enum)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{")


def definitions(text, lang):
    """name -> line number (1-based), first occurrence of each top-level definition"""
    rx = HS_DEF if lang == "hs" else C_DEF
    defs = {}
    for n, line in enumerate(text.split("\n"), 1):
        m = rx.match(line)
        if m:
            name = next(g for g in m.groups() if g)
            defs.setdefault(name, n)
    return defs


def language(path):
    return "hs" if path.endswith(".hs") else "c"


def page_name(path):
    return "source/%s.html" % path


def rel(from_page, to_page):
    return os.path.relpath(to_page, os.path.dirname(from_page) or ".")


PAGE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<link rel="icon" href="data:,">
<link rel="stylesheet" href="{up}docs.css">
</head>
<body class="{cls}">
<header class="docs-top">
  <nav><a href="{up}index.html">Console</a> <a href="{up}learn.html">How it works</a> <a href="{up}source.tar.gz">Source (.tar.gz)</a> <a href="{up}LICENSE.txt">Licence</a></nav>
</header>
{body}
</body>
</html>
"""


def file_list(files, from_page):
    out = []
    for title, blurb, paths in files:
        out.append('<h3>%s</h3><p class="note">%s</p><ul class="files">' % (html.escape(title), html.escape(blurb)))
        for p in paths:
            out.append('<li><a href="%s">%s</a></li>' % (rel(from_page, page_name(p)), html.escape(p)))
        out.append("</ul>")
    return "\n".join(out)


def main():
    root, out = sys.argv[1], sys.argv[2]
    files = []
    for title, blurb, pats in GROUPS:
        paths = []
        for pat in pats:
            paths += sorted(os.path.relpath(p, root) for p in glob.glob(os.path.join(root, pat)))
        files.append((title, blurb, paths))
    all_paths = [p for _, _, ps in files for p in ps]
    sim = next(p for p in all_paths if p.startswith("src-lib/"))

    sources = {p: open(os.path.join(root, p), encoding="utf-8").read() for p in all_paths}
    defs = {p: definitions(sources[p], language(p)) for p in all_paths}
    assumptions = []

    for p in all_paths:
        text = sources[p]
        lang = language(p)
        lines = highlight(text, lang)
        anchors = {n: name for name, n in defs[p].items()}
        rows = []
        for n, line in enumerate(lines, 1):
            ident = ' id="%s"' % anchors[n] if n in anchors else ""
            rows.append('<span class="line"%s><a class="ln" id="L%d" href="#L%d">%d</a>%s</span>'
                        % (ident, n, n, n, line))
        for n, raw in enumerate(text.split("\n"), 1):
            if "ASSUMPTION" in raw:
                assumptions.append((p, n, raw.strip().lstrip("-/*# ").strip()))
        page = page_name(p)
        up = rel(page, "x")[:-1]
        outline = " ".join('<a href="#%s">%s</a>' % (name, html.escape(name))
                           for name, _ in sorted(defs[p].items(), key=lambda kv: kv[1]))
        body = ('<main class="source"><h1>%s</h1>' % html.escape(p)
                + ('<p class="outline">%s</p>' % outline if outline else "")
                + '<pre class="code"><code>%s</code></pre></main>' % "".join(rows))  # each line is a block
        dest = os.path.join(out, page)
        os.makedirs(os.path.dirname(dest), exist_ok=True)
        with open(dest, "w", encoding="utf-8") as f:
            f.write(PAGE.format(title=html.escape(os.path.basename(p)), up=up, cls="source-page", body=body))

    # the walkthrough
    template = open(os.path.join(root, "vt100/web/learn.html"), encoding="utf-8").read()

    def link(path, name):
        if name and name not in defs[path]:
            sys.exit("mksource.py: learn.html refers to %s#%s, which does not exist" % (path, name))
        href = page_name(path) + ("#" + name if name else "")
        label = name or path
        return '<a class="ref" href="%s"><code>%s</code></a>' % (href, html.escape(label))

    def fill(m):
        kind, arg = m.group(1), m.group(2)
        if kind == "sim":
            return link(sim, arg)
        if kind == "src":
            path, _, name = arg.partition("#")
            if path not in defs:
                sys.exit("mksource.py: learn.html refers to %s, which is not rendered" % path)
            return link(path, name)
        sys.exit("mksource.py: unknown placeholder %s" % m.group())

    body = re.sub(r"\{\{(sim|src):([^}]+)\}\}", fill, template)
    body = body.replace("{{assumptions}}", "<ul class=\"assumptions\">%s</ul>" % "".join(
        '<li><a href="%s#L%d"><code>%s:%d</code></a> %s</li>'
        % (page_name(p), n, html.escape(p), n, comment_html(txt)) for p, n, txt in assumptions))
    body = body.replace("{{files}}", file_list(files, "learn.html"))
    with open(os.path.join(out, "learn.html"), "w", encoding="utf-8") as f:
        f.write(PAGE.format(title="How it works", up="", cls="learn-page", body=body))


main()
