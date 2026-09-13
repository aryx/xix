#!/usr/bin/env python3
"""Find the *real* lib_core/libc dependency closure for a .c file.

scripts/diff-c-program.sh's own dependency-closure step is a "kitchen
sink": it enumerates every source file a *full* libc.a build for a
given arch/GOOS needs (via lib_core/libc's own `mk -n -a ... install`
dry-run), then tries to compile+assemble all of them, regardless of
whether the target .c file's own call graph actually reaches them.
For a full libc.a that's ~140 files; for something like
tests/c/hello_libc/hello.c (which only calls print()+exits()), the
*real* closure turned out to be 34 files -- found by this script,
during the hello_libc stress-test investigation, see
docs/claude_notes/plan_hello_libc_linking.md.

This script computes that real, minimal closure directly: compile the
target .c file and every candidate libc source to real Plan9 assembly
text with goken's own `Nc -S` (the same real compiler stress-tested
files there use -- never xix's own, unfinished, occ/oNc), extract
every TEXT/GLOBL-defined symbol and every symbol *referenced* via
"sym+0(SB)" / "$sym(SB)", and BFS out from the target file along those
references. What's reachable is the real closure; everything else in
the "kitchen sink" list is noise for this particular target.

Usage:
    scripts/find-c-closure.py <arch> <main.c> [--out-dir DIR]

<arch> is one of: 5 (arm), 6 (amd64), 7 (arm64), v (mips), i (riscv32),
j (riscv64) -- same table as scripts/diff-c-program.sh. Only '5' is
actually exercised so far.

With --out-dir, writes the real `Nc -S` text for every file in the
closure (mangled path -> flat filename, matching diff-c-program.sh's
own convention) plus a manifest.txt listing them in closure order --
ready to feed to scripts/diff-c-program.sh-style assemble+link logic
without recompiling.

Env:
    GOKEN_ROOT   path to a built goken checkout (default: ~/goken)
"""

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path

# claude: same arch table as scripts/diff-c-program.sh -- keep in sync
# if either changes.
ARCH_TABLE = {
    "5": ("5", "arm"),
    "6": ("6", "amd64"),
    "7": ("7", "arm64"),
    "v": ("v", "mips"),
    "i": ("i", "riscv"),
    "j": ("j", "riscv64"),
}

CHIPFLOATS = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 0.5, 10.0}


def mangle(path: str) -> str:
    """Same convention as diff-c-program.sh: flatten a path into one
    filename-safe token, so files that share a basename across
    subdirectories can't collide, and each source keeps its own
    <>-scoped local-symbol namespace (see arm_port.md's "why not one
    concatenated file")."""
    return path.replace("/", "_")


def find_goken_bin(goken_root: Path) -> Path:
    mkconfig = (goken_root / "mkconfig").read_text()
    m = re.search(r"^objtype=(\S+)", mkconfig, re.M)
    if not m:
        sys.exit(f"error: couldn't find objtype= in {goken_root}/mkconfig")
    return goken_root / "ROOT" / "arch" / m.group(1) / "bin"


def enumerate_libc_files(goken_root: Path, objtype_mk: str) -> list[str]:
    """Dry-run lib_core/libc's own 'mk install' to list every source
    file a full libc.a build for this arch/GOOS needs -- the
    "kitchen sink" superset this script's BFS then narrows down."""
    libc_root = goken_root / "lib_core" / "libc"
    cmd = (
        f"cd {goken_root} && source env.sh >/dev/null 2>&1 && "
        f"cd {libc_root} && "
        f"mk -n -a 'objtype={objtype_mk}' 'cputype={objtype_mk}' 'GOOS=linux' install"
    )
    out = subprocess.run(
        ["bash", "-c", cmd], capture_output=True, text=True
    ).stdout
    files = sorted(set(re.findall(r"[A-Za-z0-9_./]+\.(?:c|s)\b", out)))
    missing = [f for f in files if not (libc_root / f).is_file()]
    if missing:
        sys.exit(f"error: mk dry-run named files that don't exist: {missing[:5]}...")
    return files


def compile_to_s(nc: Path, cflags: list[str], src: Path, out_s: Path,
                  objtype_mk: str) -> bool:
    """Compile one .c file to real Plan9 assembly text via goken's
    real Nc -S. Returns True on success. Strips stray 'warning: ...'
    lines 5c sometimes prints to *stdout* (contaminating the -S text
    itself, not stderr) -- see plan_hello_libc_linking.md."""
    proc = subprocess.run(
        [str(nc), "-S", f"-D{objtype_mk}", *cflags, "-o", "/dev/null", str(src)],
        capture_output=True, text=True,
    )
    if proc.returncode != 0:
        return False
    lines = [l for l in proc.stdout.splitlines(keepends=True)
             if not l.startswith("warning:")]
    out_s.write_text("".join(lines))
    return True


SYM_REF_RE = re.compile(
    r"\b([A-Za-z_][A-Za-z0-9_]*)\+0\(SB\)|\$([A-Za-z_][A-Za-z0-9_]*)\(SB\)"
)
SYM_DEF_RE = re.compile(r"^\s*(?:TEXT|GLOBL)\s+([A-Za-z_][A-Za-z0-9_]*)")


def refs_of(text: str) -> set[str]:
    out = set()
    for m in SYM_REF_RE.finditer(text):
        sym = m.group(1) or m.group(2)
        if not sym.startswith("."):  # local/private <>-scoped symbols
            out.add(sym)
    return out


def defs_of(text: str) -> set[str]:
    return {m.group(1) for line in text.splitlines()
            for m in [SYM_DEF_RE.match(line)] if m}


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("arch", choices=sorted(ARCH_TABLE))
    ap.add_argument("main_c", type=Path)
    ap.add_argument("--out-dir", type=Path, default=None,
                     help="save each closure file's -S text + manifest.txt here")
    args = ap.parse_args()

    if args.arch != "5":
        print(f"warning: arch '{args.arch}' is untested by this script so far "
              f"(only '5'/arm has been exercised)", file=sys.stderr)

    gk_letter, objtype_mk = ARCH_TABLE[args.arch]
    goken_root = Path(os.environ.get("GOKEN_ROOT", Path.home() / "goken"))
    goken_bin = find_goken_bin(goken_root)
    nc = goken_bin / f"{gk_letter}c"
    if not nc.is_file():
        sys.exit(f"error: {nc} not found (build goken first)")

    libc_root = goken_root / "lib_core" / "libc"
    cflags = [
        f"-I{goken_root / 'include'}",
        f"-I{goken_root / 'include' / 'ALL'}",
        f"-I{goken_root / 'include' / 'arch' / objtype_mk}",
        # claude: needed for libc's own internal headers (e.g.
        # fmt/fmtdef.h) -- missing this caused several files to
        # spuriously fail to compile for a reason unrelated to any
        # real assembler/linker gap. See
        # docs/claude_notes/plan_hello_libc_linking.md.
        f"-I{libc_root}",
    ]

    print(f"== enumerating lib_core/libc sources for objtype={objtype_mk} ==",
          file=sys.stderr)
    candidates = enumerate_libc_files(goken_root, objtype_mk)
    print(f"{len(candidates)} candidate files (the full 'kitchen sink')",
          file=sys.stderr)

    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        unit_path: dict[str, Path] = {}  # unit name -> real source path
        s_path: dict[str, Path] = {}     # unit name -> compiled -S text path

        main_unit = "main_" + mangle(args.main_c.stem)
        unit_path[main_unit] = args.main_c.resolve()
        for f in candidates:
            unit_path[mangle(f)] = libc_root / f

        print("== compiling every candidate to Plan9 assembly (Nc -S) ==",
              file=sys.stderr)
        for unit, src in unit_path.items():
            out_s = tmp / f"{unit}.s"
            if src.suffix == ".s":
                out_s.write_text(src.read_text())
                s_path[unit] = out_s
            elif compile_to_s(nc, cflags, src, out_s, objtype_mk):
                s_path[unit] = out_s
            # else: this candidate doesn't even compile standalone
            # (e.g. missing a symbol only reachable with more
            # context) -- simply unreachable for the BFS, not an
            # error worth stopping for here.

        symdefs: dict[str, str] = {}
        for unit, path in s_path.items():
            for sym in defs_of(path.read_text()):
                symdefs.setdefault(sym, unit)

        print("== BFS over symbol references from the target file ==",
              file=sys.stderr)
        closure = [main_unit]
        seen_units = {main_unit}
        seen_syms: set[str] = set()
        worklist = [main_unit]
        unresolved: set[str] = set()
        while worklist:
            unit = worklist.pop()
            for sym in refs_of(s_path[unit].read_text()):
                if sym in seen_syms or sym == args.main_c.stem:
                    continue
                seen_syms.add(sym)
                target = symdefs.get(sym)
                if target is None:
                    unresolved.add(sym)
                elif target not in seen_units:
                    seen_units.add(target)
                    closure.append(target)
                    worklist.append(target)

        print(f"\nclosure: {len(closure)} files (of {len(candidates) + 1} candidates)")
        for unit in closure:
            src = unit_path[unit]
            try:
                rel = src.relative_to(libc_root)
            except ValueError:
                rel = src
            print(f"  {rel}")
        if unresolved:
            print(f"\nunresolved symbols ({len(unresolved)}, likely data "
                  f"cells/GLOBLs this script's def-scanner missed, or "
                  f"genuinely external): {sorted(unresolved)}", file=sys.stderr)

        if args.out_dir:
            args.out_dir.mkdir(parents=True, exist_ok=True)
            manifest = []
            for unit in closure:
                dest = args.out_dir / f"{unit}.s"
                dest.write_text(s_path[unit].read_text())
                manifest.append(f"{unit}.s")
            (args.out_dir / "manifest.txt").write_text("\n".join(manifest) + "\n")
            print(f"\nwrote {len(closure)} files + manifest.txt to {args.out_dir}",
                  file=sys.stderr)


if __name__ == "__main__":
    main()
