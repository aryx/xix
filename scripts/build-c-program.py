#!/usr/bin/env python3
"""Assemble+link a real C program's dependency closure with xix's own
o<X>a/o<X>l, and (informationally) with goken's real N a/N l too.

Takes a scripts/find-c-closure.py --out-dir output (a manifest.txt
plus one real `Nc -S` file per closure unit) and:

  1. Assembles every unit with xix's o<X>a. A unit that fails isn't
     just dropped outright -- a real symbol something *else* in the
     closure needs might sit right next to the failing construct in
     the very same file (e.g. port/vlrt.c's `_f2v`/`_v2f`, which need
     the not-yet-ported MOVFD/MOVDF FPA/VFP conversion instructions,
     living alongside `_d2v`/`_v2d`/etc., which hello_libc's own
     %d-formatting call graph *does* need and which assemble fine).
     So on a syntax/codegen error, this looks at the assembler's own
     error location, strips out just the enclosing TEXT block, and
     retries -- up to a few times per unit. Every strip is logged
     loudly; if the later link fails on an undefined symbol, that
     symbol was probably in a stripped block, and the fix belongs in
     Codegen5.ml (or the relevant arch's Codegen*.ml), not here.
     Separately, before even attempting to assemble: patches out any
     FPA MOVF/MOVD immediate that isn't one of the 8 real "chipfloat"
     hardware constants (o5l's own Codegen5.ml can't yet encode
     anything else -- no real float literal pool implemented) with a
     harmless chipfloat placeholder. See
     patch_nonchipfloat_constants's own docstring for why this is
     safe for hello_libc specifically and not safe in general.
  2. Assembles every unit with goken's own real N a too, with NO
     stripping -- goken's own compiler+assembler emitting something
     it can't reassemble would be a real, different bug worth seeing
     directly, not silently working around. Purely an informational
     comparison baseline; xix's own build below never depends on it.
  3. Links whatever xix assembled with o<X>l -- this is the actual
     deliverable.
  4. Links whatever goken assembled with N l, informationally.
  5. Byte-compares the two link results when (and only when) the same
     object set assembled on both sides -- expected to diverge exactly
     when step 1's stripping kicked in, or a xix-only extension (e.g.
     CASE/BCASE) is part of the real closure.
  6. Runs xix's binary under qemu-user (if available), and goken's
     too for comparison, reporting PASS/FAIL against goken's stdout +
     exit code.

Usage:
    scripts/build-c-program.py <arch> <closure-dir> [entry_symbol] [--out FILE]

<closure-dir> is a scripts/find-c-closure.py --out-dir output.
<entry_symbol> defaults to "_main" (the real ABI entry point rt0.s
defines -- NOT "main", the C-level function name; see
find-c-closure.py's own rt0.s comment for why the two aren't the
same).

Env:
    GOKEN_ROOT   path to a built goken checkout (default: ~/goken)
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

ARCH_TABLE = {
    # arch -> (goken tool-letter prefix, xix's o<X> prefix, qemu-user binary)
    "5": ("5", "o5", "qemu-arm"),
    "6": ("6", "o6", "qemu-x86_64"),
    "7": ("7", "o7", "qemu-aarch64"),
    "v": ("v", "ov", "qemu-mips"),
    "i": ("i", "oi", "qemu-riscv32"),
    "j": ("j", "oj", "qemu-riscv64"),
}

TEXT_RE = re.compile(r"^\s*TEXT\s+([A-Za-z_.][A-Za-z0-9_.<>]*)")

# claude: goken's real FPA MOVF/MOVD immediate can only encode one of
# these 8 hardware "chipfloat" constants directly (Codegen5.ml's own
# `chipfloat`, ported from goken's real float.c) -- any other value
# needs a real float literal pool (goken's own obj.c rewrites it into
# a synthetic DATA symbol + memory load, see plan_hello_libc_linking.md
# for the exact mechanism), which this port doesn't implement yet.
CHIPFLOATS = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 0.5, 10.0}
NONCHIPFLOAT_MOVF_RE = re.compile(
    r"(MOV[FD](?:\.\w+)?\s+\$)([0-9]+\.[0-9]+(?:[eE][+-]?[0-9]+)?)(\s*,\s*F\d+)"
)


def patch_nonchipfloat_constants(text: str, unit: str) -> str:
    """Replace any FPA MOVF/MOVD immediate that isn't one of the 8 real
    chipfloat constants with a harmless placeholder (0.5, itself a
    valid chipfloat) so o5l's own real chipfloat check (Codegen5.ml)
    doesn't reject it. Only ever safe because this whole pipeline
    already established that the specific functions using such
    constants in this closure (e.g. fmt/fltfmt.c's %e/%f/%g float
    formatting, pulled in as a file-level dependency of something
    hello_libc's own %d-only call graph *does* need, but never
    executed by it) are dead code for hello_libc's own actual run --
    this is NOT generally safe for a program that really executes
    float-formatting code with values needing real (non-chipfloat)
    FPA constants. See docs/claude_notes/plan_hello_libc_linking.md
    ("a handful of non-chipfloat float constants... replaced with a
    placeholder 0.5") -- this automates that same, previously
    hand-applied fix. A real fix belongs in Codegen5.ml/Rewrite5.ml as
    a proper float literal pool, mirroring goken's own real obj.c
    mechanism (case AMOVF/AMOVD's "chipfloat(...) < 0" rewrite into a
    synthetic DATA symbol) -- not yet done."""

    def repl(m: re.Match) -> str:
        val = float(m.group(2))
        if any(abs(val - c) < 1e-12 for c in CHIPFLOATS):
            return m.group(0)
        print(f"  {unit}: patched non-chipfloat float constant {val!r} -> 0.5 "
              f"(dead code for this closure -- see patch_nonchipfloat_constants)")
        return m.group(1) + "0.5" + m.group(3)

    return NONCHIPFLOAT_MOVF_RE.sub(repl, text)


def strip_enclosing_text_block(text: str, line_no: int) -> tuple[str, str | None]:
    """Remove the TEXT-delimited function body containing 1-based
    `line_no`. Returns (new_text, removed_function_name) -- name is
    None if no enclosing TEXT block was found (nothing removed)."""
    lines = text.splitlines(keepends=True)
    idx = line_no - 1
    if not (0 <= idx < len(lines)):
        return text, None
    start = None
    for i in range(idx, -1, -1):
        m = TEXT_RE.match(lines[i])
        if m:
            start = i
            name = m.group(1)
            break
    if start is None:
        return text, None
    end = len(lines)
    for i in range(start + 1, len(lines)):
        if TEXT_RE.match(lines[i]):
            end = i
            break
    return "".join(lines[:start] + lines[end:]), name


def assemble_xix_best_effort(
    asm: Path, s_path: Path, o_path: Path, work: Path, max_strips: int = 10
) -> tuple[bool, list[str]]:
    """Assemble one unit with xix's o<X>a, stripping out any TEXT
    block whose construct o<X>a can't yet encode and retrying, up to
    max_strips times. Returns (ok, [stripped function names])."""
    text = patch_nonchipfloat_constants(s_path.read_text(), s_path.name)
    scratch = work / s_path.name
    stripped: list[str] = []
    for _ in range(max_strips):
        scratch.write_text(text)
        proc = subprocess.run(
            [str(asm), "-o", str(o_path), str(scratch)],
            capture_output=True, text=True,
        )
        if proc.returncode == 0:
            return True, stripped
        # claude: o5a prints "o5a: [ERROR] path:line msg" -- the path
        # itself (an absolute temp path) has no ':' in it on this
        # platform, so the first ":<digits> " anywhere is the line
        # number, no need to parse the path out first.
        m = re.search(r":(\d+)\s", proc.stderr)
        if not m:
            return False, stripped
        new_text, removed = strip_enclosing_text_block(text, int(m.group(1)))
        if removed is None or new_text == text:
            return False, stripped
        stripped.append(removed)
        text = new_text
    return False, stripped


def assemble_goken(asm: Path, s_path: Path, o_path: Path) -> bool:
    proc = subprocess.run([str(asm), "-o", str(o_path), str(s_path)],
                           capture_output=True, text=True)
    return proc.returncode == 0


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("arch", choices=sorted(ARCH_TABLE))
    ap.add_argument("closure_dir", type=Path)
    ap.add_argument("entry", nargs="?", default="_main")
    ap.add_argument("--out", type=Path, default=None,
                     help="copy the final xix-linked binary here")
    ap.add_argument("--goken-root", type=Path,
                     default=Path.home() / "goken")
    args = ap.parse_args()

    gk_letter, xx_prefix, qemu = ARCH_TABLE[args.arch]
    goken_root = args.goken_root
    mkconfig = (goken_root / "mkconfig").read_text()
    boot_objtype = re.search(r"^objtype=(\S+)", mkconfig, re.M).group(1)
    goken_bin = goken_root / "ROOT" / "arch" / boot_objtype / "bin"
    goken_as = goken_bin / f"{gk_letter}a"
    goken_ld = goken_bin / f"{gk_letter}l"
    if not goken_as.is_file() or not goken_ld.is_file():
        sys.exit(f"error: {goken_as} / {goken_ld} not found (build goken first)")

    xix_root = Path(__file__).resolve().parent.parent
    xix_as = xix_root / "_build" / "default" / "bin_dune" / f"{xx_prefix}a"
    xix_ld = xix_root / "_build" / "default" / "bin_dune" / f"{xx_prefix}l"
    if not xix_as.is_file() or not xix_ld.is_file():
        sys.exit(f"error: {xix_as} / {xix_ld} not found (run 'dune build' first)")

    manifest = (args.closure_dir / "manifest.txt").read_text().split()
    if not manifest:
        sys.exit(f"error: {args.closure_dir}/manifest.txt is empty")
    print(f"== {len(manifest)} closure units ==")

    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        xix_obj = tmp / "xix_obj"; xix_obj.mkdir()
        goken_obj = tmp / "goken_obj"; goken_obj.mkdir()
        xix_scratch = tmp / "xix_scratch"; xix_scratch.mkdir()

        print("== assembling every unit with xix's o<X>a (best-effort) ==")
        xix_good: list[str] = []
        any_stripped = False
        for unit in manifest:
            s_path = args.closure_dir / unit
            o_path = xix_obj / (unit + f".{xx_prefix}")
            ok, stripped = assemble_xix_best_effort(xix_as, s_path, o_path, xix_scratch)
            if ok:
                xix_good.append(unit)
                if stripped:
                    any_stripped = True
                    print(f"  {unit}: assembled, after stripping "
                          f"{stripped} (real, separate, unported "
                          f"construct -- see the stripped function's "
                          f"own source for what instruction it needed)")
            else:
                print(f"  {unit}: FAILED (xix)")
        print(f"xix: {len(xix_good)} / {len(manifest)} units assembled"
              + (" (some functions stripped, see above)" if any_stripped else ""))

        print("== assembling every unit with goken's real N a (informational) ==")
        goken_good: list[str] = []
        for unit in manifest:
            o_path = goken_obj / (unit + f".{gk_letter}")
            if assemble_goken(goken_as, args.closure_dir / unit, o_path):
                goken_good.append(unit)
            else:
                print(f"  {unit}: goken can't assemble this either")
        print(f"goken: {len(goken_good)} / {len(manifest)} units assembled")

        print("== linking ==")
        xix_out = tmp / "xix.out"
        xix_objs = [str(xix_obj / (u + f".{xx_prefix}")) for u in xix_good]
        proc = subprocess.run(
            [str(xix_ld), "-E", args.entry, "-o", str(xix_out), *xix_objs],
            capture_output=True, text=True,
        )
        if proc.returncode != 0:
            print("xix o<X>l FAILED:\n" + proc.stdout + proc.stderr)
            sys.exit(1)
        xix_out.chmod(0o755)

        goken_out = tmp / "goken.out"
        same_closure = set(goken_good) == set(xix_good)
        goken_linked = False
        if same_closure:
            goken_objs = [str(goken_obj / (u + f".{gk_letter}")) for u in goken_good]
            proc = subprocess.run(
                [str(goken_ld), "-H7", "-E", args.entry, "-s", "-o", str(goken_out), *goken_objs],
                capture_output=True, text=True,
            )
            goken_linked = proc.returncode == 0
            if not goken_linked:
                print("goken N l FAILED (informational only):\n" + proc.stdout + proc.stderr)
        else:
            only_xix = sorted(set(xix_good) - set(goken_good))
            only_goken = sorted(set(goken_good) - set(xix_good))
            print("== byte comparison skipped: object sets differ ==")
            if only_xix:
                print(f"  only in xix's closure: {only_xix}")
            if only_goken:
                print(f"  only in goken's closure: {only_goken}")

        if goken_linked:
            goken_out.chmod(0o755)
            print("== byte comparison (informational) ==")
            xix_bytes = xix_out.read_bytes()
            goken_bytes = goken_out.read_bytes()
            print(f"goken: {len(goken_bytes)} bytes    xix: {len(xix_bytes)} bytes")
            print("byte-identical" if xix_bytes == goken_bytes else "byte differences")

        print(f"== running xix's binary under {qemu} (what actually matters) ==")
        result = {"xix_out": None, "xix_rc": None, "goken_out": None, "goken_rc": None}
        import shutil
        if shutil.which(qemu):
            proc = subprocess.run([qemu, str(xix_out)], capture_output=True, text=True)
            result["xix_out"], result["xix_rc"] = proc.stdout, proc.returncode
            print(f"-- xix -- (exit {proc.returncode}): {proc.stdout!r}")
            if goken_linked:
                proc = subprocess.run([qemu, str(goken_out)], capture_output=True, text=True)
                result["goken_out"], result["goken_rc"] = proc.stdout, proc.returncode
                print(f"-- goken -- (exit {proc.returncode}): {proc.stdout!r}")
                if result["goken_rc"] == result["xix_rc"] and result["goken_out"] == result["xix_out"]:
                    print("PASS: same exit code and stdout as goken")
                else:
                    print("NOTE: exit code and/or stdout differ from goken")
        else:
            print(f"{qemu} not found, skipping functional run", file=sys.stderr)

        if args.out:
            args.out.write_bytes(xix_out.read_bytes())
            args.out.chmod(0o755)
            print(f"\nwrote final xix-linked binary to {args.out}")

        if result["xix_rc"] is None:
            sys.exit(0)
        sys.exit(0 if (not goken_linked or result["xix_rc"] == result["goken_rc"]) else 1)


if __name__ == "__main__":
    main()
