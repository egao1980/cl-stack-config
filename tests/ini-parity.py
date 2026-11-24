"""configparser parity cases for tests/ini-test.lisp (cl-stack-config#10).

    uv run --isolated --no-project python -I tests/ini-parity.py > tests/ini-parity.sexp

Stdlib only (configparser), Python >= 3.13 (allow_unnamed_section). With
`--random N [SEED]` it writes N random cases from another seed, for a local
stress run:

    uv run --isolated --no-project python -I tests/ini-parity.py --random 20000 7 > /tmp/ini.sexp
    CL_STACK_CONFIG_INI_PARITY=/tmp/ini.sexp cl-repo test

The reference is how botocore reads ~/.aws/credentials and ~/.aws/config:
configparser.RawConfigParser() with its defaults, reading the file in text
mode (cp.read(path), universal newlines). The text is written to a file as
UTF-8 and read back that way. A text with keys before the first section
(MissingSectionHeaderError with the defaults) is read again with
allow_unnamed_section=True: parse-ini keeps those keys at the root. The
expected tree is

  root     the unnamed section's own options (not [DEFAULT]'s)
  sections {name: dict(cp[name])}, so [DEFAULT]'s options are in each
           section, plus "DEFAULT": cp.defaults() when that is not empty

Some cases set a configparser option, given to parse-ini as a keyword:
inline_comment_prefixes=('#', ';') is :inline-comments t, strict=False is
:strict nil, optionxform=str is :key-case :preserve and a default_section
no file uses is :default-section nil. Text, names and values are lists of
character codes, so a CRLF checkout cannot change them. Each line is

  (:case LABEL (KEYWORD VALUE ...) (TEXT-CODES) :error)
  (:case LABEL (KEYWORD VALUE ...) (TEXT-CODES)
         (:ok ((KEY-CODES VALUE-CODES) ...)                  ; root
              ((NAME-CODES ((KEY-CODES VALUE-CODES) ...)) ...))) ; sections

Left out: a text with a key before the first section that is also a section
name (parse-ini has one table for both), and a text starting with a BOM
(parse-ini drops it; configparser finds no section header). Labels that
start with "full lowercase" need Unicode's full lowercase mapping
(str.lower(): İ is i + U+0307, final Σ is ς), which parse-ini has on SBCL.
"""
import configparser
import os
import random
import sys
import tempfile

NO_DEFAULT = "￿ no default section"

# Named cases: the rules of the README, each checked against configparser.
NAMED = [
    ("secret keeps # and ;", {},
     "[default]\naws_access_key_id = AKIDHASH\naws_secret_access_key = minio#Secret;2026\n"),
    ("a # after a blank is part of the value", {}, "[s]\nk = value # not a comment\nj = a ; b\n"),
    ("comment lines start with # or ;, after any indentation", {},
     "# c\n; c\n[s]\n  # indented comment\n\t; tab comment\nk = v\n"),
    ("option names are lowercased", {},
     "[default]\nAWS_ACCESS_KEY_ID = AKIDUPPER\nAws_Secret_Access_Key = SECRETUPPER\n"),
    ("section names keep case and blanks", {}, "[Default]\na = 1\n[default]\na = 2\n[ dev ]\na = 3\n"),
    ("the first = or : ends the name", {}, "[s]\nurl: http://h:1/p?q=1\na=b:c\nk : v = w\n"),
    ("blanks around the delimiter go, inner blanks stay", {}, "[s]\n  a  b \t=\t c  d  \n"),
    ("quotes are kept", {}, "[s]\nname = \"app db\"\nq = 'x'\n"),
    ("no interpolation", {}, "[s]\na = 100%\nb = %(a)s\nc = ${a}\n"),
    ("empty values", {}, "[s]\na =\nb:\nc = \n"),
    ("continuation lines", {},
     "[default]\ns3 =\n  addressing_style = path\n  max_concurrent_requests = 20\nregion = eu-west-1\n"),
    ("blank lines inside a value stay, trailing ones go", {}, "[s]\na = 1\n\n  2\n\n\nb = 3\n"),
    ("comment lines inside a value are skipped", {}, "[s]\na = 1\n  # c\n  2\n; d\n  3\n"),
    ("a deeper indented header continues the value", {}, "[s]\na = 1\n  [t]\n"),
    ("indented options at one level are options", {}, "[s]\n    a = 1\n    b = 2\n"),
    ("[DEFAULT] options are in every section", {},
     "[DEFAULT]\nregion = us-east-1\nk = d\n[a]\nk = a\n[b]\n"),
    ("a header ends at its last ]", {}, "[s] ; c\na = 1\n[x]y] z\nb = 2\n"),
    ("keys before the first section stay at the root", {}, "debug = true\nName = x\n[s]\na = 1\n"),
    ("CRLF and lone CR end lines", {}, "[s]\r\na = 1\r\nb = 2\rc = 3\r\n"),
    ("Unicode blanks: NBSP, ideographic space, NEL", {},
     "[s]\n a = v \nb = 1\n　more\nc = x\u0085\n"),
    ("other controls do not end a line", {}, "[s]\na = 1\x0b2\x0c3 4\n"),
    ("non-ASCII names", {}, "[Привет]\nКЛЮЧ = значение\nÉTÉ = été\n"),
    ("full lowercase: İ, ẞ, Kelvin sign, final Σ", {},
     "[s]\nİ = 1\nẞ = 2\nK = 3\nΟΔΟΣ = 4\nΣ = 5\n"),
    ("duplicate option (any case) is an error", {}, "[s]\nA = 1\na = 2\n"),
    ("duplicate section is an error", {}, "[s]\na = 1\n[s]\nb = 2\n"),
    ("[DEFAULT] may repeat", {}, "[DEFAULT]\na = 1\n[s]\n[DEFAULT]\nb = 2\n"),
    ("a line without = or : is an error", {}, "[s]\njunk\n"),
    ("an empty name is an error", {}, "[s]\n= v\n"),
    ("[] is not a header", {}, "[]\n"),
    (":inline-comments t: a prefix at the start or after a blank", {"inline": True},
     "[s] # c\na = 1 # c\nb = 1#2 ; c\nc = ;x\nd = x\t;y\n"),
    (":strict nil: duplicates merge, the last value wins", {"strict": False},
     "[s]\na = 1\nA = 2\n[s]\nb = 3\n"),
    (":key-case :preserve", {"preserve": True}, "[s]\nKey = 1\nkey = 2\n"),
    (":default-section nil: [DEFAULT] is a section", {"nodefault": True},
     "[DEFAULT]\na = 1\n[s]\nb = 2\n"),
]

BLANKS = [" ", " ", "\t", "\xa0", "　", "\x0b", "\x0c", "\x85", " "]
INDENTS = ["", "", "", "", " ", "  ", "\t", "\xa0", "　"]
ENDS = ["\n", "\n", "\n", "\r\n", "\r"]
KEYS = ["a", "A", "b", "Key", "aws_access_key_id", "AWS_SECRET_ACCESS_KEY", "k k", "é", "ÉTÉ",
        "Ж", "x.y", "[k", "k]", "#k", "k#", ";k", "%k"]
SECTIONS = ["s", "S", "default", "Default", " s ", "profile dev", "DEFAULT", "a]b", "s;c",
            "s # c", "é", "[", "]"]
VALUES = ["v", "minio#Secret;2026", "a # b", "a ; b", "#h", ";h", '"q"', "'q'", "100%",
          "%(a)s", "x=y", "x:y", "http://h:1/p?q=1#f", "", " ", "é", "\xa0v\xa0", "[t]",
          "a\tb", "k = v"]
DELIMS = ["=", ":", " = ", " : ", "\t=", "=\xa0", " :: ", "=="]
TOKENS = (["a", "B", "[", "]", "=", ":", "#", ";", '"', "%", "s", "DEFAULT", "é"]
          + BLANKS + ENDS)


def option_line(rng):
    indent = rng.choice(INDENTS)
    parts = [indent]
    if rng.random() < 0.015:
        parts.append(rng.choice(["", " "]))
    else:
        parts.append(rng.choice(KEYS))
    if rng.random() < 0.985:
        parts.append(rng.choice(DELIMS))
        parts.append(rng.choice(VALUES))
    if rng.random() < 0.2:
        parts.append(rng.choice(BLANKS) + rng.choice(["#", ";"]) + " c")
    if rng.random() < 0.15:
        parts.append(rng.choice(BLANKS))
    # Continuation lines: deeper (sometimes not) than the option, with blank
    # and comment lines between them.
    for _ in range(rng.choice([0, 0, 0, 1, 2])):
        parts.append(rng.choice(ENDS))
        if rng.random() < 0.2:
            parts.append(rng.choice(["", " ", "# c", "  ; c"]) + rng.choice(ENDS))
        deeper = indent + rng.choice(["  ", "  ", "\t", "\xa0", "\u3000", "  ", "\t", ""])
        parts.append(deeper + rng.choice(VALUES + KEYS + ["[t]", "k = v", "# not"]))
    return "".join(parts)


def header_line(rng):
    line = rng.choice(INDENTS[:6]) + "[" + rng.choice(SECTIONS) + "]"
    if rng.random() < 0.15:
        line += rng.choice([" ; c", " # c", "x", "]", " = 1"])
    return line


def line(rng):
    r = rng.random()
    if r < 0.2:
        return header_line(rng)
    if r < 0.7:
        return option_line(rng)
    if r < 0.8:
        return rng.choice(INDENTS) + rng.choice(["#", ";"]) + rng.choice(["", " c", "#", " a = 1"])
    if r < 0.95:
        return rng.choice(["", "", " ", "\t", "\xa0"])
    if r < 0.985:
        return rng.choice(["  ", "    ", "\t", "\u3000"]) + rng.choice(VALUES + KEYS)
    return rng.choice(["junk", "[]", "[ ]", "= x"])


def text_case(rng):
    if rng.random() < 0.95:
        out = []
        if rng.random() < 0.8:
            out += [header_line(rng), rng.choice(ENDS)]
        for _ in range(rng.randint(1, 7)):
            out += [line(rng), rng.choice(ENDS)]
        if rng.random() < 0.2:
            out.pop()
        return "".join(out)
    return "".join(rng.choice(TOKENS) for _ in range(rng.randint(1, 30)))


def options(rng):
    r = rng.random()
    if r < 0.7:
        return {}
    if r < 0.8:
        return {"inline": True}
    if r < 0.9:
        return {"strict": False}
    if r < 0.95:
        return {"preserve": True}
    return {"nodefault": True}


def read(path, opts, unnamed):
    kw = {"allow_unnamed_section": unnamed}
    if opts.get("inline"):
        kw["inline_comment_prefixes"] = ("#", ";")
    if opts.get("strict") is False:
        kw["strict"] = False
    if opts.get("nodefault"):
        kw["default_section"] = NO_DEFAULT
    cp = configparser.RawConfigParser(**kw)
    if opts.get("preserve"):
        cp.optionxform = str
    cp.read(path, encoding="utf-8")
    return cp


def expected(text, opts):
    """(root, sections), or None when configparser raises; "skip" for the
    left-out texts."""
    if text.startswith("﻿"):
        return "skip"
    fd, path = tempfile.mkstemp(suffix=".ini")
    try:
        with os.fdopen(fd, "wb") as out:
            out.write(text.encode("utf-8"))
        try:
            try:
                cp = read(path, opts, False)
            except configparser.MissingSectionHeaderError:
                cp = read(path, opts, True)
        except configparser.Error:
            return None
    finally:
        os.unlink(path)
    unnamed = configparser.UNNAMED_SECTION
    root = dict(cp._sections.get(unnamed, {}))
    sections = {name: dict(cp[name]) for name in cp.sections() if name is not unnamed}
    if cp.defaults():
        sections[cp.default_section] = dict(cp.defaults())
    if set(root) & set(sections):
        return "skip"
    return root, sections


def codes(s):
    return "(" + " ".join(str(ord(c)) for c in s) + ")"


def lisp_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def keywords(opts):
    out = []
    if opts.get("inline"):
        out.append(":inline-comments t")
    if opts.get("strict") is False:
        out.append(":strict nil")
    if opts.get("preserve"):
        out.append(":key-case :preserve")
    if opts.get("nodefault"):
        out.append(":default-section nil")
    return "(" + " ".join(out) + ")"


def pairs(table):
    return "(" + " ".join(f"({codes(k)} {codes(v)})" for k, v in table.items()) + ")"


def emit(label, opts, text):
    result = expected(text, opts)
    if result == "skip":
        return False
    if result is None:
        out = ":error"
    else:
        root, sections = result
        out = "(:ok " + pairs(root) + " (" + " ".join(
            f"({codes(name)} {pairs(table)})" for name, table in sections.items()) + "))"
    print(f"(:case {lisp_string(label)} {keywords(opts)} {codes(text)} {out})")
    return True


def random_cases(n, seed):
    rng = random.Random(seed)
    made = 0
    while made < n:
        if emit(f"random {seed}/{made}", options(rng), text_case(rng)):
            made += 1


if __name__ == "__main__":
    print(f";;; Generated by tests/ini-parity.py with Python {sys.version.split()[0]} "
          f"configparser; do not edit.")
    if len(sys.argv) > 1 and sys.argv[1] == "--random":
        random_cases(int(sys.argv[2]), int(sys.argv[3]) if len(sys.argv) > 3 else 1)
    else:
        for label, opts, text in NAMED:
            emit(label, opts, text)
        random_cases(600, 10)
