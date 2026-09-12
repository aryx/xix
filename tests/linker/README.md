# `*_diff/` fixture conventions

`arm_diff/`, `mips_diff/`, `riscv_diff/` and `riscv64_diff/` hold the
differential-testing corpus used to check xix's assembler+linker port
against goken's reference C toolchain, one architecture per
directory. Each `.s` file is assembled and linked by both toolchains
(via `scripts/diff-<arch>.sh`, driven by `test-<arch>.sh`), and the
two resulting executables are compared byte-for-byte (and, when a
matching `qemu-<arch>` is available, actually run).

## Filenames

`<category>_<specifics>[_case<N>[_<M>...]][_check].s`, e.g.
`mem_move_case6_7.s` or `float_int_move_case30_31_check.s`.

- **category/specifics** come first and should be readable on their
  own to someone unfamiliar with the target's `asm.c`/optab case
  numbering -- `branch`, `shift_imm`, `float_arith`, `lui`, etc.
- **`case<N>`** (optional) cross-references the `(* case N: ... *)`
  comment in the corresponding `Codegen*.ml` file and the matching
  entry in `docs/claude_notes/todo_<arch>_port.org`, for readers who
  *do* want to trace a fixture back to the reference toolchain's own
  code. Several case numbers are joined with `_` when one fixture
  exercises more than one (`case23_25`, `case30_31`).
- **No arch suffix** (no trailing `_arm`/`_mips`/`_riscv`/`_riscv64`)
  -- the directory already says which architecture a fixture is for.
- Fixtures with no specific case number (baselines like
  `hello_linux.s`/`exit_linux.s`, or general/relocation tests like
  `addr.s`/`call.s`/`kitchen_sink.s`) skip the `case<N>` part
  entirely.

## The `_check` suffix

A `..._check.s` fixture is the companion to a same-named non-`_check`
fixture, used when the straightforward, byte-identical version of a
test isn't possible: goken's own assembler does something this port
deliberately doesn't replicate (real instruction scheduling filling a
delay/hazard slot instead of a plain NOP, a code-layout optimization,
dead-code elimination after an unconditional jump, etc -- each such
gap is documented where it was found, usually in
`docs/claude_notes/todo_<arch>_port.org` and/or the fixture's own
header comment).

A `_check` fixture is **not** byte-compared; instead it self-checks
its own arithmetic at runtime (typically via a taken/not-taken branch
into distinct exit codes) and only the *behavior* (exit code, or
matching trap/crash) is expected to agree between goken and xix. It
is deliberately excluded from the corresponding `test-<arch>.sh`'s
`CASES` list (which only ever contains byte-identical fixtures) --
run it directly with `scripts/diff-<arch>.sh` instead.

## Entry point

Every fixture uses `TEXT _start(SB), $...` and is linked with `-E
_start` on both sides (not the linkers' own default, `_main`).
