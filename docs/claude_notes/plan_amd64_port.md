# Porting the amd64 toolchain (6a/6l) against goken, byte-equal

**Status: second checkpoint reached.** `o6a`/`o6l` exist, and two
fixtures assemble+link to executables **byte-identical** to goken's
real `6a`/`6l` output, with identical `qemu-x86_64` behavior:
`tests/linker/amd64_diff/hello_linux.s` (goken's own real
`tests/s/hello_arch/hello_linux_amd64.s`, copied verbatim; prints
"Hello, world", exit 0) and `cmp_jcc.s` (CMPQ + JEQ/JNE/JLT/JGE, exit
42). `./test-amd64.sh` runs both. Zero regressions across all 4
already-complete ports' own full suites (`test-arm.sh` 54/54,
`test-mips.sh`, `test-arm64.sh`, `test-riscv.sh`, `test-riscv64.sh`)
and `make test` (134/134), both before and after this second batch.

This follows the same "free rein between two review checkpoints"
shape the ARM64 port used (see [[arm64-linker-port]] memory): this is
the first checkpoint (basic structure + one real fixture working),
for review before going further.

## Why this arch is different from every other one ported so far

amd64 is the first genuinely CISC, variable-length-instruction arch
in this whole differential-testing project (ARM32/MIPS/ARM64/RISC-V
are all fixed-4-byte-instruction RISC machines). Three consequences,
each a real, non-cosmetic design point:

1. **The shared linker's executable writer assumed 4-byte words.**
   `linker/Execgen.ml`'s text-segment parameter was `Types.word list`,
   and `Codegen{5,7,i,v}.ml`'s own `gen` functions hard-asserted
   `List.length instrs * 4 = size`. Generalized (with the user's
   explicit sign-off first, since it touches all 4 completed ports):
   `Execgen.gen` now takes `Types.byte array` for text uniformly (it
   already did for data), with a new `Types.bytes_of_words` helper
   flattening each existing arch's own `word list` into bytes,
   *endian-aware* (`Arch.endian_of_arch`) -- MIPS is genuinely
   big-endian here, so an earlier draft that hardcoded `Little` would
   have silently corrupted every `ovl` output; caught before it ever
   landed. `Codegen6.ml` produces `Types.byte array` directly, no
   word/bitfield packing at all (`Bits.int32`'s own `sanity_check_32`
   hardcodes a 32-bit budget -- doesn't fit a 1-15-byte instruction
   stream). Verified: full 4-arch regression suite still green after
   this change, before amd64 itself was even wired up.

2. **A real, pre-existing bug found while wiring this arch up**:
   `Arch.endian_of_arch` had `X86 -> Endian.Big` and
   `Amd64 -> Endian.Big` -- both wrong (amd64/x86 are always
   little-endian; there is no big-endian mode for either). Latent
   until now since nothing had exercised these two `Arch.t`
   constructors before. Fixed.

3. **SP is a real register here, not a virtual pseudo-register.** On
   every other arch, Plan9's "SP" token is purely virtual --
   `x(SP)`-style local-variable addressing is resolved by the
   *linker*, once a TEXT's real frame size is known, and the actual
   hardware stack-pointer register has its own separate name (R13 on
   ARM32, etc). On amd64, "SP" (`D_SP` in goken's own
   `include/objexec/6.out.h`) *is* the real hardware register --
   confirmed against goken's own `hello_linux_amd64.s`, which uses
   bare "SP" as an ordinary arithmetic operand (`SUBQ $16,SP`) and
   "0(SP)"/"8(SP)" as already-concrete indirect-with-displacement
   addressing, needing no linker-side rewriting at all. Only "x(FP)"
   is the traditional Plan9 *virtual* addressing convention here
   (goken's `D_PARAM`), resolved the same way every other arch
   resolves its own frame-relative entity -- except the fixed bias
   added on top of the frame size is "+8" here (the return address
   goken's real `CALL` pushes onto the stack in hardware), not some
   arch-specific link-register-save-slot size. See `Ast_asm6.ml`'s own
   prelude for the full reasoning, and `Parser_asm6.mly`'s `reg`/`gen`
   comments for how the grammar keeps the two forms (`TSP` as a plain
   register vs. `TFP` as `pointer`'s virtual case) apart without any
   ambiguity.

   A direct consequence: amd64 needs **no auto-generated
   prologue/epilogue** at all (`Rewrite6.ml` is a genuine no-op) --
   `CALL`/`RET` push/pop the return address in hardware, and any real
   stack adjustment is already explicit source-level instructions
   (goken's own `hello_linux_amd64.s` does its own "SUBQ $16,SP" /
   "ADDQ $16,SP"), unlike ARM32/MIPS/RISC-V/ARM64's own
   frame-size-driven synthesis. Similarly **no literal pool**
   (`Layout6.ml` is a plain PC-accumulation walk, no pool-splicing
   machinery at all) -- LEAQ's absolute address and any move-immediate
   this checkpoint supports are both encoded inline in the instruction
   stream, not through a separate pool the way ARM32/ARM64/MIPS/RISC-V
   all need one.

## Grounding

Unlike some earlier ports (which mostly read one specific dispatch
function in goken's C source to settle one specific question), this
one leaned on goken's real source throughout, since x86-64's encoding
table has no RISC-family analog anywhere else in this project to lean
on intuition from:
- `~/goken/assemblers/6a/{a.h,a.y,lex.c}` -- the real grammar/lexer.
- `~/goken/linkers/6l/{obj.c,optab.c,span.c,asm.c}` -- the real
  codegen, especially `span.c`'s `doasm()`/`asmandsz()`/`asmand()`
  (REX/ModRM/SIB construction) and `optab.c`'s per-mnemonic
  operand-class dispatch tables.
- Every encoding rule was additionally byte-verified against goken's
  own real `6a`/`6l` output for `hello_linux_amd64.s` (`objdump`
  wasn't available for cross-arch ELF on this host; qemu-x86_64's own
  `-d in_asm` trace and manual REX/ModRM/SIB decoding by hand were
  used instead) *before* being ported, not derived from reading the C
  source alone and trusted blind.

## What's covered (Codegen6.ml)

Only what `hello_linux_amd64.s` itself needs -- see `Ast_asm6.ml`'s
own prelude for the full scope statement:
- `Arith` (ADD/SUB/XOR): register destination, immediate source (only
  when the immediate fits signed 8 bits, goken's `Yi8`/opcode `0x83`)
  or register source (goken's `Zr_m`, one real opcode per mnemonic:
  `0x01`/`0x29`/`0x31`).
- `Move` (MOVQ only, no B/W/L-suffixed forms): register<->register,
  register<->memory (goken's `Zr_m`/`Zm_r`, opcodes `0x89`/`0x8b`),
  and immediate-to-register-or-memory when the immediate fits signed
  32 bits sign-extended (goken's `Zilo_m`, opcode `0xc7 /0`).
- `Lea` (LEAQ, address-of-global only): goken's own "built-in LEAQ"
  `Zaut_r` row, opcode `0x8d`, always the absolute-disp32-via-SIB
  addressing shape (goken's non-PIE amd64 default -- confirmed
  `HEADTYPE`-gated in `span.c`'s `asmandsz()`, macOS PIE uses
  RIP-relative instead, not implemented here).
- `Call` (direct only, to a label): opcode `0xe8` + rel32. Always
  exactly 5 bytes regardless of the actual displacement (unlike ARM's
  own branch-range story), so no chicken-and-egg sizing problem here.
- `Cmp` (CMPQ, immediate or register): goken's `ycmpl`-shaped compare,
  same operand-role-order quirk documented in `Ast_asm6.ml`'s own `Cmp`
  comment (the ModRM r/m operand is the *first* written operand here,
  unlike `Arith`). Immediate form only wired for signed-8-bit
  (`Zm_ibo`/`0x83 /7`), register form via `Zm_r`/`0x39` only (not the
  reverse-direction `Zr_m`/`0x3b` row).
- `Jcc` (JEQ/JNE/JLT/JGE/JGT/JLE/JCS/JCC/JHI/JLS): goken's `yjcond`,
  **short (rel8) form only** -- see "Real bugs/quirks" below for why
  the near form (and hence real branch-distance relaxation) isn't
  worth chasing yet.
- `Ret` (`0xc3`), `Syscall` (`0x0f 0x05`).
- `Jmp` (JMP, direct only): goken's `yjmp`, short (rel8) form, same
  scope as `Jcc`. Wired and believed correct, but **not yet covered by
  a byte-identical fixture** -- see "Real bugs/quirks" below, goken's
  own linker turned out to apply real control-flow transformations
  (dead-code elision, loop rotation) to code around an unconditional
  jump, neither of which is worth replicating for this checkpoint;
  every fixture so far avoids the patterns that trigger them.
- The static/local symbol `foo<>` suffix, `NAME = value` constant
  definitions -- **not** wired (see "Known gaps" below; not needed by
  this fixture, but real, findable gaps the same way they were for
  ARM -- see [[hello-libc-integration-test]]).

Deliberately not wired (all raise `Todo` rather than emit wrong
bytes): the imm32 arith form (`0x81`), the imm=0 move optimization
(`Zclr`/`0x31`) and true-64-bit-immediate move (`Ziq_rp`/`0xb8`), any
memory base register other than SP (BP/R13 need a real ModRM/SIB
special case for `[rip+disp32]` this port doesn't have), R8-R15
(parseable already via the shared "R"+digit lexer rule, but nothing in
`Codegen6.ml` threads REX.B/.R/.X yet), byte/word/long-suffixed
arithmetic and moves, floating point/SSE, conditional jumps, indirect
CALL.

## Real bugs/quirks found via differential testing against goken directly

- **`Arch.endian_of_arch`'s `X86`/`Amd64 -> Endian.Big`** -- see
  point 2 above. A real, if latent, pre-existing bug.
- **goken's real `6l` needs an explicit `-S` flag to emit ELF section
  headers at all** -- confirmed in `~/goken/linkers/liblk/elf.c`
  (`elf32`/`elf64` both gate every section-header write behind
  `if(debug['S'])`). Without it, goken's own 6l silently writes
  `e_shoff=0`/`shnum=0` (verified via `readelf -h`), a real,
  `6l`-specific default that `5l`/`7l`/`8l` apparently don't share
  (their own existing `diff-*.sh` scripts never pass `-S` and still
  get section headers from goken) -- not root-caused further (would
  need reading `linkers/6l/asm.c`'s own call sequence vs `7l`'s), just
  worked around by adding `-S` to `scripts/diff-amd64.sh`'s own goken
  invocation (xix's `o6l`, like every other arch's own linker here,
  always emits section headers, so the comparison needs both sides to
  actually do so to be meaningful).
- **A build-artifact-staleness gotcha, a new variant of
  [[arm64-linker-port]]'s own "dune build --force can still be
  insufficient" note**: `_build/default/bin_dune/o6a`/`o6l` (the path
  every existing `diff-*.sh` script uses) went stale after an edit +
  `dune build --force` -- the file was still there, right size-ish,
  but from a build *before* the fix, while `_build/default/linker/
  Main.exe` (the real build output) and the top-level `bin_dune/`
  symlink (-> `_build/install/default/bin/`) were both already fresh.
  `dune build @install --force` did *not* fix it either. Worked around
  by having `scripts/diff-amd64.sh` use the `bin_dune/` symlink path
  instead -- confirmed that one always tracks the freshest `Main.exe`.
  Not root-caused (which of dune's install-related rules actually
  populates `_build/default/bin_dune/`, and why it doesn't reliably
  fire on every rebuild, is still unknown) -- if this bites another
  port, prefer `bin_dune/oXX` over `_build/default/bin_dune/oXX`
  outright rather than re-debugging it.
- **goken's real `6l` deletes unreachable code around an unconditional
  jump, and the jump itself once its target collapses to "the next
  instruction"** -- found while first trying to test `Jmp`/`JMP`:
  a "JMP L; <dead code>; L:" fixture (dead code padding added to force
  the near/rel32 form past the short form's 127-byte range) assembled
  to *zero bytes* for the JMP and everything it skipped, confirmed
  down to a minimal repro. Root cause not fully chased (plausible
  given goken's own Go-toolchain lineage: real compiler-style
  optimizations, not just assembly), but confirmed to also extend to
  **loop rotation**: a `loop: cmp;jeq exit; body; jmp loop` structure
  gets goken's own comparison duplicated into a trailing conditional
  branch, eliminating the unconditional backward jump entirely (a
  genuine "while → do-while with a duplicated leading check"
  transformation). Both are specific to the *unconditional* jump --
  confirmed a conditional (`Jcc`) forward skip's own fallthrough is
  *not* elided (it's always statically reachable, values aside).
  Consequence: byte-identical fixtures for `Jmp` need to avoid both
  patterns, which rules out the two most natural ways to exercise it
  in a small test -- `cmp_jcc.s` therefore doesn't test `Jmp` at all
  yet (see "Suggested phase plan" below).
- **Confirmed goken's real relaxation between a Jcc/JMP's short
  (2-byte, rel8) and near (5/6-byte, rel32) forms is a genuine
  multi-pass sizing problem** (span.c's `Zbr`/`Zjmp` cases pick based
  on the actual resolved distance) -- not attempted; only the short
  form is wired (`cmp_jcc.s`'s own four `Jcc` checks all use it).

## Suggested phase plan (next checkpoints)

Roughly in the order a next real fixture would need them, mirroring
how ARM32/ARM64's own follow-up phases were sequenced:
1. Byte/word/long-suffixed (`B`/`W`/`L`) arithmetic and moves, and the
   `-0x81`/imm32 arith form -- likely needed almost immediately by any
   fixture beyond a hello-world.
2. R8-R15 (REX.B/.R/.X threading through every encoder in
   `Codegen6.ml` -- already parseable, per this file's own "What's
   covered" list).
3. Indirect CALL/JMP (through a register or memory). Direct
   conditional jumps landed this checkpoint (`Cmp`/`Jcc`, short form
   only); a real fixture for `Jmp` itself and Jcc/Jmp's near-form
   relaxation are still open -- see "Real bugs/quirks" above for why
   both are genuinely harder than they looked (goken's own dead-code
   elision and loop rotation, and real multi-pass distance-dependent
   sizing, respectively).
4. `foo<>` static/local symbols and `NAME = value` constants --
   same real, findable-in-goken's-own-hand-written-`.s` gaps
   documented for ARM in [[hello-libc-integration-test]]; likely to
   surface again the moment a real (not synthetic) amd64 `.s` file is
   tried.
5. True 64-bit immediates (`Ziq_rp`) and the imm-zero optimization
   (`Zclr`) for MOVQ.
6. RIP-relative addressing (needed the moment a fixture targets a
   non-Linux `HEADTYPE`, or if this project ever wants position-
   independent amd64 output) and the BP/R13 ModRM special case.
7. Floating point/SSE -- large, deferred indefinitely absent a
   concrete need.
