# Porting the amd64 toolchain (6a/6l) against goken, byte-equal

**Status: fifth checkpoint reached.** `o6a`/`o6l` exist, and five
fixtures assemble+link to executables **byte-identical** to goken's
real `6a`/`6l` output, with identical `qemu-x86_64` behavior:
`hello_linux.s` (goken's own real hello-world, exit 0), `cmp_jcc.s`
(CMPQ + JEQ/JNE/JLT/JGE, exit 42), `r8_r15.s` (R8-R15 across every
instruction, exit 12), `movl_arith.s` (32-bit MOVL/ADDL/CMPL,
including the register- vs memory-destination MOVL-immediate split and
the Zclr $0 optimization, exit 135), `indirect_call_jmp.s` (indirect
CALL/JMP through a register, exit 7). `./test-amd64.sh` runs all five.
Zero regressions across all 4 already-complete ports' own full suites
(`test-arm.sh` 54/54, `test-mips.sh`, `test-arm64.sh`, `test-riscv.sh`,
`test-riscv64.sh`) and `make test` (134/134) after every batch.

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

See `Ast_asm6.ml`'s own prelude for the full scope statement:
- `Arith` (ADD/SUB/XOR, **Q and L width**): register destination,
  immediate source (only when the immediate fits signed 8 bits,
  goken's `Yi8`/opcode `0x83`) or register source (goken's `Zr_m`, one
  real opcode per mnemonic: `0x01`/`0x29`/`0x31`).
- `Move` (MOVQ/MOVL, no B/W-suffixed forms): register<->register,
  register<->memory (goken's `Zr_m`/`Zm_r`, opcodes `0x89`/`0x8b`),
  `$0`-to-register (goken's `Zclr` self-XOR optimization -- **both**
  `ymovq` *and* `ymovl` have this row; a real bug in this port's own
  first attempt assumed only `ymovq` did, see "Real bugs/quirks"), and
  immediate-to-register-or-memory when the immediate fits signed 32
  bits sign-extended -- MOVQ always via `Zilo_m`/`0xc7 /0`, but MOVL's
  own table puts the simpler `Zil_rp`/`0xb8+reg` (no ModRM at all, same
  family as `Ziq_rp`) *before* `Zilo_m`, so a register destination
  takes that path instead and only a memory destination falls through
  to `Zilo_m` -- a real, non-obvious shape difference from MOVQ.
- `Lea` (LEAQ, address-of-global only, 64-bit only): goken's own
  "built-in LEAQ" `Zaut_r` row, opcode `0x8d`, always the
  absolute-disp32-via-SIB addressing shape (goken's non-PIE amd64
  default -- confirmed `HEADTYPE`-gated in `span.c`'s `asmandsz()`,
  macOS PIE uses RIP-relative instead, not implemented here).
- `Call` (direct only, to a label): opcode `0xe8` + rel32. Always
  exactly 5 bytes regardless of the actual displacement (unlike ARM's
  own branch-range story), so no chicken-and-egg sizing problem here.
- `Cmp` (CMPQ/CMPL, immediate or register): goken's `ycmpl`-shaped
  compare, same operand-role-order quirk documented in `Ast_asm6.ml`'s
  own `Cmp` comment (the ModRM r/m operand is the *first* written
  operand here, unlike `Arith`). Immediate form only wired for
  signed-8-bit (`Zm_ibo`/`0x83 /7`), register form via `Zm_r`/`0x39`
  only (not the reverse-direction `Zr_m`/`0x3b` row). No `Yi0`/`Zclr`
  row exists for CMP in goken's own `ycmpl` (unlike MOVQ/MOVL), so no
  special-casing needed there.
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
- **R8-R15**, across every instruction above: REX.R (whichever
  register sits in ModRM.reg) and REX.B (ModRM.rm or SIB.base, or the
  opcode-embedded register for MOVL/MOVQ's `Zil_rp`/`Ziq_rp` forms),
  computed by `rex_opt`/`rex_b_of_resolved_gen`. For a 32-bit (L)
  instruction the whole REX byte is optional and omitted entirely when
  neither R8-R15 register is involved (confirmed against real 6a:
  "ADDL BX,AX" is just `01 d8`, no prefix byte at all). REX.X (SIB
  index) is never set -- no indexed addressing implemented.
- **Indirect `Call`/`Jmp`** (through a register only, no memory
  operand): goken's own `ycall`/`yjmp` `Zo_m64` row, opcode `0xff /2`
  (CALL) or `0xff /4` (JMP), plain ModRM with no mandatory REX (a
  low-register indirect call/jmp needs no prefix at all -- confirmed
  "CALL BX" -> `ff d3`). Reuses the shared `A.branch_operand`'s
  existing `IndirectJump` constructor (no new AST needed) -- and
  confirmed real 6a accepts *both* a bare register ("CALL BX") *and* a
  parenthesized one ("CALL (BX)") as the target, so this arch's own
  grammar wires both.
- The static/local symbol `foo<>` suffix, `NAME = value` constant
  definitions -- **not** wired (not needed by any fixture yet, but
  real, findable gaps the same way they were for ARM -- see
  [[hello-libc-integration-test]]).

Deliberately not wired (all raise `Todo` rather than emit wrong
bytes): the imm32 arith form (`0x81`), true-64-bit-immediate move
(`Ziq_rp`'s own full-8-byte-immediate sub-case, `0xb8`), any memory
base register other than SP (BP/R13 need a real ModRM/SIB special case
for `[rip+disp32]` this port doesn't have), indexed addressing
(SIB.index, hence REX.X), byte/16-bit-suffixed (B/W) arithmetic and
moves, floating point/SSE, indirect CALL/JMP *through memory* (only
through a register is wired), Jcc/Jmp near-form relaxation.

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
- **A real bug in this port's own first attempt at MOVL's immediate
  form**: assumed (by loose analogy with CMP's own `ycmpl`, which
  genuinely has no `Yi0` row) that `ymovl`, like `ymovq`, would need
  `MOVL $0,R` special-cased separately -- but actually just skipped
  wiring `Zclr` for *either* width at first, on the wrong assumption
  neither table had the row. Both do (confirmed reading `ymovl`'s own
  table more carefully, then empirically: `MOVL $0,AX` assembles to
  `31 c0`, a bare self-XOR, not `Zilo_m`). Fixed for both `Q_` and `L_`
  uniformly, one `Move (width, Right (Int 0), GReg r)` case.
- **R8-R15's REX.R/.B computation confirmed exactly against real 6a
  byte output** for every instruction shape wired so far, including
  the trickiest one (`ADDQ R8,R9`, needing REX.R *and* REX.B set
  simultaneously since one register sits in ModRM.reg and the other in
  ModRM.rm) and MOVL's own opcode-embedded-register forms (`MOVL
  $9,R9` -> REX.B extends the `0xb8+reg` opcode byte itself, not a
  ModRM field, confirmed `41 b9 09 00 00 00`). No surprises here --
  goken's own reg[]/regrex[] scheme (`obj.c`) turned out to already
  generalize cleanly to every ModRM/opcode-embedding shape used.
- **A real forward-reference bug in `Lea`**: "LEAQ later_proc(SB),R"
  naming a *procedure* (TEXT symbol) declared further down the same
  file crashed with `Not_found` during `Layout6.ml`'s sizing pass --
  unlike a DATA global (fully resolved upfront by the separate
  `Layout.layout_data` pass before `Layout6.layout_text` even starts),
  a TEXT symbol's own `SText2` entry is only added *incrementally* as
  the sizing walk reaches each `TEXT` pseudo-op, so a forward reference
  genuinely isn't in the table yet when the referencing `LEAQ` is
  sized. Fixed the same way as `Call`/`Jmp`/`Jcc`'s own real_pc-
  dependent values: size computed eagerly with a placeholder address
  (safe, since `encode_rm`'s absolute-address shape is always the same
  6 bytes regardless of the value), the real lookup deferred into
  `binary`'s own thunk (only ever invoked by `Codegen6.gen`'s pass,
  which runs *after* `Layout6.layout_text` has fully finished and
  every TEXT symbol -- forward or not -- is in the table). Found via
  `indirect_call_jmp.s`'s own forward-referenced `exitnow`.
- **Confirmed real 6a's indirect CALL/JMP takes a bare register with
  no parens** ("CALL BX", not "CALL (BX)") -- a real difference from
  the parenthesized `(R1)` convention this arch's own `ireg` grammar
  rule was copied from ARM64's template with. Both forms turned out to
  be valid real 6a syntax once actually checked (confirmed
  independently), so this arch's grammar now accepts both.
- **Confirmed a same-function local label's address can't be taken
  with LEAQ** in real 6a ("LEAQ label(SB),R" for a label inside the
  same TEXT errors) -- only real global symbols (other TEXT/DATA
  entries) work as LEAQ's target. Shaped `indirect_call_jmp.s` around
  this (both indirect-jump targets are genuine separate TEXT globals).

## Suggested phase plan (next checkpoints)

Roughly in the order a next real fixture would need them, mirroring
how ARM32/ARM64's own follow-up phases were sequenced:
1. `foo<>` static/local symbols and `NAME = value` constants --
   same real, findable-in-goken's-own-hand-written-`.s` gaps
   documented for ARM in [[hello-libc-integration-test]]; likely to
   surface again the moment a real (not synthetic) amd64 `.s` file is
   tried.
2. A real fixture for `Jmp` itself (still not covered, per "What's
   covered" above) and Jcc/Jmp's near-form relaxation, and
   memory-indirect (not just register-indirect) CALL/JMP -- see "Real
   bugs/quirks" above for why the first two are genuinely harder than
   they looked (goken's own dead-code elision and loop rotation, and
   real multi-pass distance-dependent sizing, respectively).
3. Byte/16-bit-suffixed (`B`/`W`) arithmetic and moves, and the
   `0x81`/imm32 arith form -- the remaining width gaps (Q/L landed
   this checkpoint).
4. True 64-bit immediates (`Ziq_rp`'s own full-width sub-case).
5. RIP-relative addressing (needed the moment a fixture targets a
   non-Linux `HEADTYPE`, or if this project ever wants position-
   independent amd64 output), indexed addressing (SIB.index/REX.X),
   and the BP/R13 ModRM special case.
6. Floating point/SSE -- large, deferred indefinitely absent a
   concrete need.
