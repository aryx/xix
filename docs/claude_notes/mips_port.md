# Porting the MIPS toolchain (ova/ovl) against goken, byte-equal

Status: **complete**. Every `linkers/vl/asm.c` case (0-48, non-contiguous:
0,1,2,3,4,5,6,7,8,9,10,11,12,13,16,18,19,20,21,22,23,24,25,26,27,28,30,31,
32,33,34,35,36,37,38,39,40,41,42,45,46,47,48) is either ported, confirmed
to need no new code, or confirmed dead/unreachable in goken itself. See
"Open issues" at the end for the one thing still genuinely unresolved.
The real, full `hello.c`+`lib_core/libc` closure additionally links and
runs correctly under `qemu-mips`, matching real goken's own reference
build byte-for-byte on stdout and exit code -- see "hello_libc
integration test" below for the ~20 further gaps (and 2 confirmed real
bugs) only that scale of test could find.

## Goal

Same as `docs/claude_notes/arm_port.md`, mirrored for MIPS:
assemble+link the same `.s` with both goken's va/vl (Plan 9 MIPS
assembler/linker) and xix's ova/ovl, and require the final executables
to be byte-identical. Scope is MIPS assembler + linker only, same
reasoning as the ARM doc for why the compiler stays out of it (occ is
headed towards ~/c--'s shared backend, not goken's per-arch compilers).

Read `arm_port.md` first -- this doc only calls out what's
MIPS-specific or different from the ARM story; the general approach
(harness shape, syncweb rules, where changes land, goken flag
adjustments allowed) is identical and not repeated here.

## Grounding: how goken's va/vl map onto xix

Unlike 5l (split into codegen.c/datagen.c/layout.c/...), goken's vl
is NOT split -- it's still the older monolithic asm.c (the numbered
case-N switch, our codegen.c equivalent) plus separate pass.c/span.c/
optab.c/obj.c/list.c/sched.c/noop.c. The case-N switch (asmout-style)
is in `linkers/vl/asm.c`, cases 0-48 (not contiguous, see above).

xix's `Codegenv.ml` started with 11 cases already tagged (1,3,4,5,7,8,
11,18,19,35,36) -- MIPS started from a similar place ARM did, not from
zero.

goken's va/vl binaries live at `~/goken/ROOT/arch/boot-gcc/bin/{va,vl}`
(same `GOKEN_ROOT` convention as every other arch).

## A genuine architecture fork, not a bug: endianness

goken's va/vl produce big-endian MIPS (MSB). xix's
`lib_core/commons/Arch.ml` had deliberately chosen little-endian for
Mips (comment: "the PS1 is a little-endian R3000"), for PS1/embedded-
little-endian-MIPS compatibility. These are genuinely incompatible
bit patterns for anything wider than a byte -- there is no way to be
byte-equal with goken while staying little-endian.

Decision (asked and answered): switch xix's Mips to big-endian
(`Endian.Big`) to match goken and get real byte-equal testing, dropping
PS1 compatibility as a goal for now. Goal is to run on qemu-mips
(big-endian) and match what goken produces, not qemu-mipsel.

That flip immediately reproduced a segfault/crash the code had a
standing TODO about ("if put Big here I get a segfault with ovl, why
???" -- Arch.ml, before this change). Root cause: `linker/Execgen.ml`
hardcoded `Endian.Little.output_32` for writing the TEXT section's
instruction words, in both the A_out and Elf branches, regardless of
`config.arch`. So flipping just the ELF header's byte-order marker
(which `Elf.ml` already correctly derives from `Arch.endian_of_arch`)
produced a file claiming MSB while the actual instruction words were
still LSB -- any consumer decoding them as big-endian (qemu-mips)
read garbage 32-bit words and hit an illegal instruction. Fixed by
deriving the output functions from `Arch.endian_of_arch config.arch`
instead of hardcoding Little. `Datagen.ml` was already correctly
parametrized by endian -- `Execgen.ml`'s text-section writer was the
only hardcoded spot found.

Verified: `tests/linker/mips_diff/hello_linux.s` is byte-identical
against goken and runs correctly under qemu-mips (prints "Hello,
world", exits 0) on both sides. `make test` still 134/134 after (the
compiler's codegen tests exercise `Arch.Mips` too, via
`Test_compiler.ml` -- worth double-checking after any further
Mips-endian-adjacent change, since those tests don't check machine
code bytes, only compiler stdout/stderr, so they wouldn't have caught
this).

## Harness

`scripts/diff-mips.sh` + `test-mips.sh`, structurally identical to
`scripts/diff-arm.sh` + `test-arm.sh` (same bash-not-Testo reasoning,
same `GOKEN_ROOT` convention, same `-s` to strip goken's native symbol
table). Two differences from the ARM harness:
- No `-H` flag needed on either side: vl defaults `HEADTYPE` to 7 (ELF)
  already (obj.c: `if(HEADTYPE == -1) HEADTYPE = 7`), unlike 5l
  which defaults to Plan 9 native and needs `-H7` explicit. ovl's
  `CLI.ml` also always defaults `header_type` to "elf" regardless of
  arch, so this was already consistent on the xix side.
- qemu-mips (big-endian), not qemu-mipsel, now that Mips is
  `Endian.Big`. qemu-mips/qemu-mipsel/qemu-mips64/qemu-mips64el are
  all installed on this host.
- Default INITTEXT for MIPS+ELF: goken's vl uses `0x00400000+HEADR`;
  xix's `CLI.ml` already had exactly `0x400000 + header_size` for
  `Arch.Mips` (matches, was already correct, unlike the endian bug).

## Test corpus sources

`tests/linker/mips_diff/`, sibling to `tests/linker/arm_diff/` (see
that folder's rationale in the ARM doc -- same applies here: mostly
assembler codegen + minimal exec-layout being exercised, still filed
under `tests/linker/` since it's ova+ovl's combined output). See
`tests/linker/README.md` for the fixture naming/`_check` conventions.

First fixture: `hello_linux.s`, copied from goken's
`tests/s/mini/hello_linux.s` (the simpler version -- xix's own
`tests/assembler/hello_linux.s` adds an extra "never_executed"
TEXT+JAL+WORD block for nm/onm testing, not needed for this baseline).
Registers: R2=$v0 (syscall number and return value), R4-R10=$a0-$a6
(syscall args), R30=$sp-ish SB-bias register (MIPS' equivalent of
ARM's R12/BIG mechanism -- `MOVW $setR30(SB), R30` is the MIPS
analogue of `MOVW $setR12(SB), R12`).

Entry point is `_start` (not the linkers' default `_main`), same as
every ARM fixture -- pass `-E _start` on both sides.

## Port log

Case numbers refer to `linkers/vl/asm.c`'s `switch(o->type)`.

### Phase 0 -- harness

- **`base_and_offset_of_entity`'s Param/Local +4 split**: confirmed,
  via case 26's `lacon_case26.s` fixture, to be the exact same bug as
  ARM's (see the case 26 entry below for the full writeup and the
  goken `vl -a` numbers). Fixed by swapping the +4 onto Local instead
  of Param.
- **Diff harness script** (assemble+link both sides, `cmp -l`,
  qemu-mips): `scripts/diff-mips.sh` + `test-mips.sh`, mirrors the ARM
  ones. No `-H` flag needed. qemu-mips, not qemu-mipsel, since Mips is
  now `Endian.Big`.
- **A real, previously-flagged endianness bug**: see "A genuine
  architecture fork, not a bug: endianness" above.

### Phase 0.5 -- baseline corpus (constructs already ported)

- **`hello_linux.s`**: `cmp -l` clean + qemu-mips. Copied from goken's
  `tests/s/mini/hello_linux.s`. PASS: byte-identical (4112 bytes both
  sides) on the first real run, once the endian bug above was fixed.
  Both sides print "Hello, world" and exit 0 under qemu-mips.
- **`exit_linux.s`**: `cmp -l` clean + qemu-mips. Copied from goken's
  `tests/s/exit/exit_linux.s` (no data section). Caught a real bug:
  case 18's JMP (the expansion of a leaf RET) emitted just the jump,
  missing MIPS' mandatory branch-delay-slot instruction -- goken's
  `noops()` (vl/noop.c) always fills it, with `NOR R0,R0,R0` (funct
  0x27) specifically, not the all-zero `SLL R0,R0,0` some other MIPS
  toolchains use as NOP (checked against goken's actual output
  byte-for-byte, first guess of a plain zero word was wrong). Fixed
  in `Codegenv.ml` (case 18 now emits 2 words, size=8). PASS:
  byte-identical (326 bytes) + matching qemu-mips exit code (42)
  after. Verified: MIPS hello-world fixture, ARM baseline (5
  fixtures), and `make test` (134/134) all still pass.
- **JAL's (case 11) missing delay slot** -- functional bug, not just
  bytes. Confirmed via `call.s` (hand-written, no goken equivalent
  existed): missing delay-slot handling on JAL was a real correctness
  bug, not just a byte-cosmetic one. Without an explicit slot, the
  CALLER's next instruction (setting up the exit code, in this
  fixture) got silently consumed as the delay slot -- executing right
  after JAL but *before* the callee ran -- and its effect was then
  clobbered by the callee's own register use, producing the wrong
  exit code (1 instead of 0) on xix while goken's build ran
  correctly. Fixed by giving JAL the same NOP treatment as case 18
  (shared `nop` constant, size=8, hoisted the NOP definition out of
  case 18 into a shared let). Exit codes now match on both sides.
  Byte-equality is NOT fully reached for `call.s` though (4 bytes
  still differ, offsets 180-184): goken's sched.c is a real
  instruction scheduler (dependency analysis via
  `depend()`/`conflict()`/`offoverlap()` in vl/sched.c) that hoists
  the callee's first instruction into the delay slot instead of a
  plain NOP, and duplicates it so it's still there for other entry
  paths. Decided NOT to port this: it's a genuine scheduler
  implementation, much bigger than any single case, for a payoff
  limited to a few bytes on call-adjacent code that's already
  functionally correct. `call.s` is kept as a fixture but
  deliberately left out of `test-mips.sh`'s CASES list for this
  reason (run it manually with `scripts/diff-mips.sh` to see the
  diff). Revisit only if a future fixture's byte-equality actually
  depends on it.
- **`offset_to_R30` (address-of-global)** -- and why it was never
  started. Mirrors the ARM immrot/BIG story (`Codegen5.ml`), but
  simpler in the end. `offset_to_R30` had the same kind of stub ARM's
  `offset_to_R12` did ("LATER? x - BIG optimisation"), and the
  dispatch code had two dead-end `failwith "TODO: ..."` branches for
  any nonzero R30-relative offset -- confirmed by `kitchen_sink.s`
  (hand-written, two GLOBLs at different offsets, same setup that
  found the ARM bug), which crashed ovl outright rather than just
  producing wrong bytes.
  Root-caused by reading goken's actual `span.c aclass()` (the
  D_ADDR/SDATA case): the fast R30-relative-ADD path's condition is
  `instoffset >= -BIG && instoffset < BIG && instoffset != 0`, and
  goken's own `linkers/vl/l.h` sets `BIG = 0` (an old value of 32766
  is left commented out right above it) -- collapsing the condition
  to `instoffset >= 0 && instoffset < 0`, which no integer ever
  satisfies. So this fast path is permanently dead in goken *itself*
  on MIPS, unlike ARM where BIG=4092 could occasionally still make it
  reachable for a large enough data segment. That's exactly why it
  was never started here: matching goken means never taking it, for
  any offset, so there was nothing to port beyond "always fall
  through to the absolute-constant (lu+or) load" -- which is what the
  fix now does unconditionally, replacing both `failwith` branches
  with a comment pointing at this explanation. Heavily commented in
  `Codegenv.ml` itself too, since neither the ARM nor the MIPS version
  of this mechanism was well understood going in.
  PASS: byte-identical (both `addr.s` and `kitchen_sink.s`, the
  latter on the very first run after the fix) + matching qemu-mips
  behavior. Verified: full MIPS + ARM baselines and `make test`
  (134/134) all still pass.

### Phase 1 -- asm.c case backlog

- **Case 1** `mov[v] r1,r2 ==> OR r1,r0,r2`
- **Case 3** `mov $soreg, r ==> or/add $i,o,r` (fixed a real bug):
  `constant_kind`'s range check was `i <= 0xffff` with no lower
  bound -- trivially true for ANY negative i in OCaml, so a genuinely
  out-of-range negative constant (needing case 19/24's lu-based
  expansion, not ported for a plain literal) would have silently gone
  through the direct-fit path instead of correctly failing loudly.
  Fixed to `i >= -0x8000 && i <= 0xffff`, matching goken's actual
  ZCON/SCON/ADDCON/ANDCON range (same boundary reasoning as the case
  4/10 fix below).
  Separately, also fixed which opcode case 3 emits: the old code
  always used ADDU regardless of i's sign, but goken picks ORI
  specifically for the ANDCON range (0x8000-0xffff) -- ADDU would
  sign-extend e.g. `$0x8000` into -32768 instead of +32768. Confirmed
  against goken directly (`vl -a`): 0x7fff uses ADDIU, 0x8000/0xffff
  use ORI. New `movw_imm_opcode` helper picks OR vs ADD(W,U)
  accordingly.
  Added `movw_andcon_case3.s` (MOVW $32768 and $65535, the two ANDCON
  boundary values) -- no branches needed here (unlike case 4/10's
  `immcon_case4_10.s`), so this one is fully byte-identical, not just
  functionally correct.
- **Case 5** `syscall`
- **Case 11** `jmp lbra`
- **Case 18** `jmp [r1],0(r2)`
- **Case 19** `mov $lcon,r ==> lu+or`
- **Case 2** `add/sub r1,[r2],r3` -- generic register-register-register
  arith (ADD/SUB/AND, plus OR/XOR/SGT via goken's oprange-aliasing in
  span.c's `buildop()` equivalent, same pattern as ARM's `buildop()`).
  `oprrr_arith_opcode` already had all these opcodes encoded (used by
  case 1's OR trick already), so this was just wiring up the match
  arm -- explicitly excludes SLL/SRL/SRA even though
  `oprrr_arith_opcode` also handles those, since case 9 ("asl
  r1,[r2],r3") uses a *different*, swapped operand order for shifts.
  Added `arith_rrr_case2.s` (ADD/SUB/AND chained into the exit code,
  avoiding plain register-to-register MOVW which isn't ported yet
  either). PASS byte-identical + PASS same qemu-mips exit code.
- **Case 9** `asl r1,[r2],r3` -- shift-by-register (SLL/SRL/SRA), same
  shape as case 2 but with the shift-amount register and the
  value-being-shifted register swapped in the encoding call (goken's
  `OP_RRR(oprrr(p->as), r, p->from.reg, p->to.reg)` vs case 2's
  `OP_RRR(oprrr(p->as), p->from.reg, r, p->to.reg)`) -- exactly the
  gap flagged when case 2 was ported. Added `shift_reg_case9.s` (SLL
  then SRL chained into the exit code: 1<<4=16, 16>>2=4). PASS
  byte-identical + PASS same qemu-mips exit code.
- **Case 6** `beq r1,[r2],sbra` -- conditional branches: BEQ/BNE
  (2-register form, from AST's BEQ/BNE) and the whole
  ABGEZ/ABGEZAL/ABGTZ/ABLEZ/ABLTZ/ABLTZAL family (1-register-vs-zero,
  from AST's Bxx) -- goken's case 6 covers all of these uniformly
  with one formula (`OP_IRR(opirr(p->as), v, p->from.reg, p->reg)`,
  optab.c: all share oprange 6), split here into two match arms only
  because BEQ/BNE's middle register is a real optional operand
  (defaults to R0) while Bxx's "register" slot is always occupied by
  the BCOND sub-opcode bits instead (`BCOND(x,y)=(x<<19)|(y<<16)`,
  selecting BGEZ/BGEZAL/BLTZ/BLTZAL within `SP(0,1)` -- see
  `opirr_bxx_opcode`). Needed a new `op_irr_no_r3` helper for the Bxx
  arm: reusing plain `op_irr` with r3=0 would add a *second* Bits.t
  tuple at bit offset 16 on top of BCOND's own (y,16) contribution,
  which `Bits.sanity_check_32` rejects (two entries at the same
  offset) even though the actual encoded value is identical (goken's
  C passes `p->reg==NREG` there, which OP_IRR's `&31` masks down to 0
  -- a no-op contribution, just not expressible as a *second* tuple
  in this representation).
  The branch offset itself needed a new helper (`gbranch_offset`),
  distinct from `gbranch_static` (JMP/JAL's *absolute* word address):
  case 6 is PC-relative (`v = (p->cond->pc - pc - 4) >> 2` in goken),
  the -4 being the mandatory delay slot.
  Also completed case 11 alongside this (it was only handling JAL's
  variant, `JAL {Absolute _}`; optab.c shows plain unconditional
  `JMP label` -- as opposed to case 18's `JMP (r)` indirect form --
  shares the exact same oprange 11, just without linking).
  Added `branch_case6.s` (BEQ, BNE, BGTZ, BLTZ, each on its taken
  path, accumulating 1+2+4+8=15 into the exit code). PASS
  functionally (exit code 15 both sides) but NOT byte-identical
  (goken: 414 bytes, xix: 430 bytes, diff spread across several delay
  slots) -- same root cause as `call.s`'s known JAL diff: goken's
  sched.c hoists real instructions into branch delay slots instead of
  this port's plain NOPs. Deliberately not chasing this -- kept as a
  fixture but left out of `test-mips.sh`'s CASES list, run manually
  with `scripts/diff-mips.sh`.
- **Case 4 (fixed a real bug) + case 10** `add $con,[r1],r2`.
  `Codegenv.ml`'s case 4 previously had a standing TODO ("C_ADD0CON
  vs C_ANDCON generate different opcodes") and just always emitted a
  single `op_irr`, truncating the immediate via `land 0xffff`
  regardless of its actual size -- a real, previously-latent bug:
  e.g. `ADD $0x8000,R1,R2` would have silently encoded as ADDI
  `$-0x8000` (wrong sign) instead of the REGTMP-based +0x8000 goken
  actually uses. Root-caused via goken's `span.c aclass()/cmp()`:
  case 4's needed class (C_ADD0CON) is reached whenever the constant
  fits ADDI/ADDIU's native sign-extended 16-bit immediate directly --
  actual class ZCON, SCON, *or* ADDCON, i.e. the *full*
  [-0x8000, 0x7fff] range (confirmed empirically against goken with
  `vl -a`, single instruction at both ends, including exactly
  -0x8000). Outside that range (ANDCON, [0x8000, 0xffff]) needs case
  10's REGTMP+OR expansion instead. Case 4 is now guarded by this
  exact range and case 10 added right after it for the ANDCON half;
  further out (UCON/LCON, cases 25/23) now correctly falls through to
  the generic "not handled" error instead of silently emitting wrong
  bytes.
  Note: case 10's goken C code also has an AADDU/sign-extend variant
  for *negative* values (`if(v<0) r=AADDU`), but that's only ever
  reached for AAND (AND-immediate isn't ported at all yet), so only
  the OR/positive half of case 10 is implemented.
  Added `immcon_case4_10.s`: ADD at all four boundary corners
  (32767/-32768 for case 4, 32768/65535 for case 10), each pair
  combined via ADD/SUB (case 2) into a small, self-checked delta via
  BEQ. Hit a genuine, unrelated quirk along the way: comparing
  against a plain `MOVW $-32768,R` (rather than combining boundary
  values arithmetically) makes *goken's own* assembler emit a
  literal-pool SB-relative load instead of a direct immediate for
  that one exact literal, and the resulting executable then segfaults
  under qemu-mips on goken's side -- nothing to do with case 4/10's
  ADD path itself (confirmed separately via `vl -a` that ADD $-32768
  correctly stays a single ADDI on goken); just avoided in the
  fixture. PASS functionally (exit code 7 both sides) but NOT
  byte-identical, same already-documented delay-slot-scheduler
  reason as `branch_case6.s` -- kept out of `test-mips.sh`'s CASES.
- **Case 12** `movbs r,r` / **case 13** `movbu r,r` -- sign/zero-
  extending byte or half register move, no memory involved (Move1's
  first xix codegen match arms). Sign-extend (MOVB/MOVH) has no
  dedicated instruction in goken: SLL then SRA by 24 (byte) or 16
  (half), pushing the sign bit up to bit 31 then arithmetic-shifting
  it back down. Zero-extend (MOVBU/MOVHU) is a plain AND-immediate
  mask (0xff/0xffff). New `op_srr` helper for `OP_SRR(op,s,r2,r3)`
  (shift-immediate encoding, shared with case 16). Encoding confirmed
  against goken directly via `vl -a` before writing the fixtures.
  Added `movbh_case12_13.s` (all four forms on the same -1 register
  value, combined via SUB into a branch-free, fully byte-identical
  check) and `movbh_case12_13_check.s` (a BEQ-based functional check
  of the sign-extend side specifically). PASS byte-identical
  (`movbh_case12_13.s`) + PASS functionally (both).
- **Case 16** `sll $c,[r1],r2` -- shift-by-immediate, the natural
  sibling of case 9 (shift-by-register) and reusing `op_srr` (added
  for case 12). Same scoping choice as case 9: only the W-sized
  (32-bit) SLL/SRL/SRA opcodes, not goken's V-sized/`vshift()`
  ALAST-aliased >=32 path. Added `shift_imm_case16.s` (SLL then SRL
  then SRA chained into the exit code: 1<<4=16, 16>>2=4, 4>>1=2).
  PASS byte-identical + PASS same qemu-mips exit code.
- **Case 20** `mov lohi,r` / **case 21** `mov r,lohi` / **case 22**
  `mul r1,r2` -- MFHI/MFLO, MTHI/MTLO, and MUL. Required *new AST*,
  unlike every other MIPS case so far: goken's D_HI/D_LO
  pseudo-registers had no representation at all in `Ast_asmv.ml`.
  Added `type lohireg = HI | LO` and a new `LoHi of lohireg`
  constructor on `vgen`, reusing Move2 rather than adding a whole new
  instr case -- goken's own optab.c only lists HI/LO under
  AMOVW/AMOVV, so this is a Move2 (register-move) concern, not
  Move1's. Bumped `Object_file.version` (9->10). Grammar: new `TLOHI`
  token, plus a `vlgen` alternative (`vlgen: ... | TLOHI { Left (LoHi
  $1) }`) -- discovered `vlgen` wraps `lgen`, which only derives from
  `gen | ximm`, so LoHi needed its own alternative there too, not
  just on `vgen`. Lexer: "HI"/"LO" keywords.
  Case 22 (MUL) has no destination register at all in real MIPS --
  the result always lands in HI/LO (goken's optab.c row for AMUL
  declares "to" as C_NONE, REGZERO fills that encoding slot
  unconditionally); xix's own grammar puts the 2-operand form's
  second register into ArithMul's `dest` slot rather than `r_opt`,
  but `r_opt|||rt` recovers the same value either way. Only
  MUL(W,_) is implemented -- goken's optab.c has no row at all for
  ADIV/AREM (nor their V variants); AMULU aliases the same oprange as
  AMUL, so both MUL signs work.
  Discovered along the way (empirically, via `vl -a`, not fully
  characterized): goken's sched.c pads two NOPs around certain
  MUL-result HI/LO read/write transitions -- e.g. MUL then MFLO then
  MFHI then MTLO needs no padding, but MUL then MFLO then (an
  unrelated buffer instruction) then MFHI *does*, non-monotonically
  in the buffering. Same already-documented, out-of-scope scheduler
  subsystem as every branch/call delay slot, not chased further.
  Added three fixtures: `mul_case22.s` (MUL alone, no HI/LO readback,
  byte-identical), `mullohi_case20_21.s` (MTLO/MTHI/MFLO/MFHI
  round-trip with *no* MUL at all, byte-identical), and
  `mullohi_case20_22_check.s` (MUL immediately followed by MFLO,
  self-checked with BEQ -- inherits both the MUL/HI-LO scheduler gap
  *and* BEQ/JMP's own delay-slot gap).
- **Case 24** `mov $ucon,,r ==> lu r` (case 23/25 turned out to need a
  different plan, see next entry). UCON (low 16 bits all zero,
  magnitude outside case 3's range): a single LUI, no OR needed. Case
  3's own guard was converted from an internal `constant_kind`
  Option-match to a direct `when` guard so case 24 (and the LCON
  fallthrough) can sit as separate, later match arms on the same
  pattern shape.
  Tried porting case 19 for a *plain integer literal* too (not just
  the already-working Address-of-Global path) using the same LU+OR
  shape -- turned out to be WRONG: confirmed via `vl -a` directly
  that a genuine LCON literal (nonzero low 16 bits, magnitude beyond
  ANDCON) makes goken's `va` rewrite the MOVW into a 4-instruction
  sequence loading the constant's value from a synthesized
  SB-relative data symbol (a literal pool, analogous to ARM's)
  instead of synthesizing it arithmetically. This is a genuine
  *assembler*-side mechanism (needs a MIPS literal pool in `ova`),
  not a `Codegenv.ml`/linker one -- case 19's LU+OR is only correct
  for the Address-of-Global path, which goes through a different,
  non-literal-pool-rewriting `aclass()` branch in goken. Reverted
  that arm; a plain out-of-range literal now falls through to the
  pre-existing generic "not handled" error instead of emitting wrong
  bytes.
  Added `movw_ucon_case24.s` (a positive and a negative UCON value).
  PASS byte-identical + PASS same qemu-mips exit code.
- **Case 23** `add $lcon,r1,r2 ==> lu+or+add` / **case 25** `add/and
  $ucon,[r1],r2 ==> lu $con,t; add t,[r1],r2` -- confirmed via `vl -a`
  beforehand that ADD's immediate forms do NOT hit MOVW case 19's
  literal-pool surprise, so both ported exactly as case 4/10's
  REGTMP-based pattern already suggested: case 25 (UCON, low 16 bits
  zero) is LU into REGTMP then the real op (no OR needed); case 23
  (LCON, the fallback for anything not ADD0CON/SCON/ANDCON/UCON) adds
  an OR step to merge in the low bits before the real op. goken's C
  also diags if `p->to.reg` or `p->reg` is REGTMP itself ("cant
  synthesize large constant") -- not replicated, a real
  assembler-error case no fixture exercises.
  Added `add_bigimm_case23_25.s` (one ADD at each boundary, combined
  via SUB -- case 2). PASS byte-identical + PASS same qemu-mips exit
  code.
- **Case 26** `mov $lsext/auto/oreg,,r2 ==> lu+or+add` -- address-of-
  local/param, the SP-relative sibling of the Address-of-Global case
  19. No small-offset fast path here either: like `offset_to_R30`
  (address-of-global), goken's C_SACON fast path is gated on
  `instoffset >= -BIG && instoffset < BIG`, permanently dead since
  BIG=0 on MIPS -- so this always takes the generic 3-instruction
  LU+OR+ADDU path, reusing the pre-existing `base_and_offset_of_entity`
  helper.
  That helper had a real, previously-flagged-as-unverified bug: the
  "+4 for the caller/RLINK-slot adjustment" was on the Param branch
  instead of Local (Local had no adjustment at all) -- the exact same
  bug shape as the confirmed ARM one in `base_and_offset_of_indirect`.
  Confirmed via `vl -a` on a fixture with a named FP/SP-relative local
  and param (frame=$8192): goken computes offset 8192 for `x-8(FP)`
  (Local) and 8204 for `y+8(SP)` (Param), only consistent with
  autosize=8196 and the +4 on Local. Fixed by swapping the +4.
  Fixing this exposed a SECOND, separate real bug while testing:
  `Rewritev.ml`'s TEXT-prologue rewrite always emitted an RLINK save
  whenever autosize > 0, even for a LEAF function with a nonzero
  frame (locals but no calls) -- goken's noop.c (the ATEXT case)
  always reserves the +4-adjusted SP space, but only actually saves
  RLINK `if(!(curtext->mark & LEAF))`. Fixed by gating the RLINK-save
  half of the prologue on the same `is_leaf` table step1 already
  builds. Left a documented gap for the analogous RET-side case (a
  leaf function with autosize > 0 should do a direct `ADD
  $autosize,SP; JMP RLINK`, not this code's load-from-memory+
  restore+jmp) since no fixture exercises RET on a leaf-with-locals
  function yet.
  Added `lacon_case26.s` (a named FP local and SP param, combined via
  SUB). PASS byte-identical (only after both fixes -- the
  `base_and_offset_of_entity` swap alone left an 8-byte diff from the
  spurious RLINK save) + PASS same qemu-mips exit code. Verified
  `call.s` (non-leaf, run manually) still its known, pre-existing,
  unrelated 4-byte scheduler diff.
- **Case 32** `fadd fr1,[fr2],fr3` (ADD_/SUB_/MUL_/DIV_ only) / **case
  33** `fabs fr1,fr3` (ABS_/NEG_) -- unlike case 27/28/30/31/34, these
  already fit the existing ArithF AST (freg * freg option * freg) with
  no new AST needed. New helpers `fpf`/`fpd` (goken's FPF/FPD macros
  -- SP(2,1) plus the single/double "fmt" field, bit 21: 16=single,
  17=double) and `op_frrr` (OP_FRRR's r1@16/r2@11/r3@6 layout,
  distinct from `op_rrr`'s r1@16/r2@21/r3@11). CMPEQ_/CMPGT_/CMPGE_
  also alias into case 32's oprange in goken, but left unimplemented:
  their optab row has C_REG (not C_FREG) on the second operand and
  C_NONE on the dest -- a real comparison writes to an implicit FP
  condition flag, not a normal freg, so it doesn't fit case 32's
  ArithF-with-a-real-dest shape as cleanly.
  Added `float_arith_case32_33.s` (ADDF/ADDD/SUBF/MULF/DIVF/ABSF/NEGD)
  -- byte-comparison only, since there's no way yet to load a
  meaningful value into a float register. PASS byte-identical + PASS
  same (trivial, floats all start at 0) qemu-mips exit code.
- **Case 30** `movw r,fr` (MTC1) / **case 31** `movw fr,r` (MFC1) --
  required *new AST*, unlike case 32/33: none of `Ast_asmv.ml`'s
  existing operand types could represent a float register on one
  side of a Move2. Added `GFReg of freg` to `vgen`. `Object_file.version`
  bumped (10->11). Grammar: `vgen`/`vlgen` both got a plain `freg`
  alternative. New `op_mfc_mtc` helper for
  `OP_RRR(SP(2,1)|(4-or-0<<21), rint, 0, rfloat)` -- the 4-vs-0
  sub-field selects direction; goken's "r2=0" is really that same
  sub-field, not a real middle operand, so it's baked into the opcode
  prefix rather than a separate Bits.t tuple.
  Discovered (via `vl -a`) that MTC1/MFC1 have a mandatory MIPS I
  COP1-transfer delay slot, just like a branch: an isolated MTC1/MFC1
  right before SYSCALL gets a plain NOP padded after it, and goken's
  sched.c fills the slot with a real hoisted instruction when one's
  eligible. Same already-documented, out-of-scope scheduler gap as
  every other one -- emits a plain nop unconditionally (size=8), same
  pattern as case 6/11/18's branches.
  Added `float_int_move_case30_31.s` (MTC1 and MFC1 each right before
  SYSCALL, nothing eligible to hoist, byte-identical) and
  `float_int_move_case30_31_check.s` (MTC1 then MFC1 of the *same*
  register, round-tripping 42, self-checked with BEQ). PASS
  byte-identical (`float_int_move_case30_31.s`) + PASS functionally
  (both -- the non-check one actually traps under qemu-mips on *both*
  sides identically, a `vl -a`-confirmed match rather than a real
  functional check, since nothing there sets up a valid syscall).
- **Case 34** `mov $con,fr ==> or/add $i,r,r2` -- float-constant load,
  reusing case 3's exact OR-vs-ADDU choice (`movw_imm_opcode`) into
  REGTMP, then MTC1'd into the float register. goken's optab.c only
  lists ADDCON/ANDCON for this case -- no UCON/LCON variant here;
  those instead reuse case 35/36's oprange per optab.c, left as an
  open question (whether that's genuinely shared or a distinct
  sub-case).
  Surprising finding, contradicting the case 30/31 precedent: unlike
  a standalone MTC1 (which always gets a trailing delay-slot nop),
  this fused ORI+MTC1 sequence does NOT -- confirmed via `vl -a`
  directly (`MOVW $42,F0` right before SYSCALL is just 2 words, no
  NOR/NOP). Not fully understood (goken's delay-slot marking likely
  keys off the original, pre-expansion Prog, not each emitted word)
  but empirically consistent: if something immediately reads the
  float register afterward, *that* instruction's own delay slot
  (case 31's nop) still produces the right total byte count.
  Added `float_const_case34.s` (both the ADDCON and ANDCON halves,
  byte-identical) and `float_const_case34_check.s` (loads 42 into F0,
  reads it back via MFC1, self-checks with BEQ). PASS byte-identical
  (`float_const_case34.s`) + PASS functionally (both).
- **Case 7** `mov r,soreg ==> sw o(r)` (fixed a real, previously
  untested bug) / **case 8** `mov soreg,r ==> lw o(r)` (same) / **case
  35/36** (Indirect variant added) -- discovered while starting to
  port case 27/28 (float memory access): case 7/8's *existing*,
  already-committed implementation was wrong for any nonzero offset,
  case 35/36 was missing an entire addressing variant, and two
  separate assembler-level gaps (a missing keyword and missing
  grammar rules) had made this whole family untestable via real
  source until now.
  Root cause (case 7/8): `span.c`'s `aclass()` for a plain D_NONE
  register-indirect operand (`Indirect(reg,off)`, no `$` symbol)
  returns C_ZOREG only for offset==0 exactly, else C_SOREG if
  `instoffset >= -BIG && instoffset < BIG` -- unsatisfiable for ANY
  offset since BIG=0, the same dead-fast-path shape as
  C_SECON/C_SACON but never previously flagged for this specific
  D_NONE/indirect-value case. So C_SOREG is ALSO permanently dead:
  case 7/8's single-instruction fast path is only ever reached for
  offset==0; any other offset needs case 35/36's REGTMP-based
  expansion instead (confirmed via `vl -a`: `MOVW R1,4(R5)` takes the
  4-instruction path, `MOVW R1,0(R5)` the 1-instruction one). Fixed
  by guarding case 7/8 to offset==0 literally, and adding a new
  Indirect-based match arm to case 35/36 (previously Entity-only) for
  any other offset.
  Separately: every LOAD in this family (case 8/36/27, not the STORE
  cases 7/35/28) has its own mandatory MIPS I load-delay-slot hazard
  -- confirmed via `vl -a` for each variant (ZOREG/LOREG/Entity, int
  and float). This was ALSO a previously-undetected gap in case 36 --
  fixed by adding a plain nop after every load variant.
  Two assembler-level gaps found and fixed along the way, both
  genuinely blocking rather than a codegen concern:
  - "MOVF"/"MOVD" were entirely missing from `Parse_asmv.ml`'s keyword
    table (only "MOVW"/"MOVV" were mapped to TMOVE2) -- *every*
    MOVF/MOVD instruction, including ones already "working" via case
    32/33/34, had never actually been run through a real parse. Fixed
    by adding both keywords.
  - `gen` (`Parser_asmv.mly`) had no production for plain
    register-indirect (`N(Rx)`, no symbol) or bare-entity (`sym(SB)`/
    `x(FP)`/`y(SP)`, no `$`) addressing at all -- only `GReg` via a
    bare register. Added `con TOPAR reg TCPAR -> Indirect` and
    `name -> Entity`. This means case 35/36 (Entity-based store/load)
    had *also* never been exercised via real assembly source before
    now, only ever reachable through `Rewritev.ml`'s own internally-
    synthesized instructions.
  Added `mem_move_case7_8.s` (int store, ZOREG + LOREG,
  byte-identical) and `mem_move_case7_8_check.s` (functional load
  round-trip, self-checked with BEQ). PASS byte-identical
  (`mem_move_case7_8.s`) + PASS functionally (both).
- **Case 27** `mov [sl]ext/auto/oreg,fr ==> lwc1 o(r)` (F__ only) /
  **case 28** `mov fr,[sl]ext/auto/oreg ==> swc1 o(r)` (F__ only) --
  float memory access, reusing the same ZOREG/LOREG/Entity split and
  REGTMP expansion just fixed/completed for case 7/8/35/36 above
  (this is what surfaced those bugs in the first place). New
  `op_irr_raw` helper (like `op_irr` but taking a plain int instead of
  a typed reg for r3, since the destination/source here is a float
  register). F__ (single-precision) only -- D__ needs two word
  transfers (to freg+1 and freg, a double occupies a consecutive
  float-register pair) and `opirr_mem` has no D__ encoding yet either,
  left as a follow-up. goken's C also diags if the base register is
  REGTMP itself for the slow-path arms; not replicated, a real
  assembler-error case no fixture exercises.
  Added `float_move_case27_28.s` (float store, ZOREG + LOREG -- the
  Entity/GLOBL variant is NOT covered by a fixture, see "Open issues"
  below) and `float_move_case27_28_check.s` (functional float load
  round-trip through MFC1). PASS byte-identical (`float_move_case27_28.s`)
  + PASS functionally (both).
- **Case 37** `movw r,mr` (MTC0/DMTC0) / **case 38** `movw mr,r`
  (MFC0/DMFC0) -- coprocessor-0/MMU register moves. Required *new
  AST* (added `MReg of mreg` to `vgen`; `Object_file.version` bumped
  11->12) but the token/lexer infrastructure (TM/TMx, "M5" and "M(5)"
  syntax) already existed, unused -- only the `mreg`/`vgen`/`vlgen`
  grammar rules themselves were missing. New `op_mc0` helper, same
  shape as case 30/31's `op_mfc_mtc` but SP(2,0) instead of SP(2,1)
  and a size-dependent (W__/V__) sub-field: 4/5 for write (case 37),
  0/1 for read (case 38).
  Case 37 (write) needs no delay slot. Case 38 (read) has its own,
  *different* hazard: goken's noop.c has a dedicated, unconditional
  2-NOP special case for any AMOVW/AMOVV whose source is D_MREG or
  D_FCREG (inserted directly in noop.c's own marking pass, not
  decided by `sched()`'s usual hoisting) -- confirmed via `vl -a` for
  an isolated MFC0 (2 NOPs even with an eligible instruction available
  right after), but NOT fully characterized when another MREG/FCREG
  instruction sits nearby.
  MTC0/MFC0 are privileged (kernel-mode) instructions, illegal under
  qemu-mips' user-mode emulation -- confirmed to crash identically on
  both goken's and xix's output.
  Added `cop0_move_case37_38.s` (case 37 writes only, byte-identical)
  and `cop0_move_case37_38_check.s` (case 37+38 chained). PASS
  byte-identical (`cop0_move_case37_38.s`) + PASS same qemu-mips
  behavior (both). Left case 41/42 (FCR moves) as a follow-up.
- **Case 40** `word` -- a raw constant word embedded in TEXT. Already
  handled generically by `Codegen.default_rules` (shared across all
  archs, matched at the very top of `rules` via `T.WORD`), no
  MIPS-specific code needed. Added `word_case40.s` just to confirm
  that shared path produces byte-identical output on MIPS too. PASS
  byte-identical + PASS same qemu-mips behavior (an intentional
  segfault on both sides).
- **Case 41** `movw r,fcr` (MTCC1) / **case 42** `movw fcr,r` (MFCC1)
  -- floating point control register moves (e.g. FCR31), the
  follow-up deferred from case 37/38. Required new AST (`FCReg of
  fcrreg` on `vgen`; `Object_file.version` bumped 12->13); the
  fcrreg token/lexer infrastructure (TFCR/TFCRx, "FCR31" syntax)
  already existed unused. New `op_cfc_ctc` helper: same SP(2,1) family
  as case 30/31's MTC1/MFC1, but a different sub-field (2 for MFCC1,
  6 for MTCC1).
  Case 41 (write) does a dummy MFCC1 read into REGZERO first (a real
  MIPS I FCR hazard workaround already in goken's C), then the real
  MTCC1 write -- no delay slot needed after. Case 42 (read) is a
  single instruction but gets the same unconditional 2-NOP D_FCREG-
  source special case as case 38's MFC0.
  Unlike case 37/38 (MTC0/MFC0, privileged/kernel-only), FCR31 is
  accessible from user mode -- confirmed to actually run correctly
  under qemu-mips, not just trap identically.
  Added `fcr_move_case41_42.s` (write only, byte-identical) and
  `fcr_move_case41_42_check.s` (write+read round trip, self-checked
  with BEQ). PASS byte-identical (`fcr_move_case41_42.s`) + PASS
  functionally (both).
- **Case 39** `rfe ==> jmp+rfe` -- kernel-only "return from
  exception": JR (r) followed by a fixed RFE instruction (goken's
  `oprrr(ARFE)` = MMU(2,0), no operands). RFE already existed in the
  AST (`RFE of branch_operand`); only needed a grammar rule (`TRFE
  branch`, reusing the same `branch` nonterminal JMP/JAL already use
  for their indirect form). Unlike every other jump/branch, RFE
  itself fills JR's own mandatory delay slot by design -- no extra
  nop needed (confirmed via `vl -a`: exactly 2 words). Reuses case
  18's exact OP_RRR shape (r1=0, r2=target, r3=0).
  Hit a real, not-worth-chasing surprise while testing: goken's own
  assembler drops any code textually following an RFE with no label
  pointing at it (a genuine, intentional dead-code elimination for
  unreachable code after an unconditional computed jump) -- a first
  fixture with `RFE (R5)` followed by `SYSCALL` came out 8 bytes
  smaller on goken's side because the SYSCALL was silently dropped.
  Not implemented here -- fixed by just not putting anything after
  RFE in the fixture, matching how real Plan9 kernel code uses it
  anyway (RFE last in an exception handler).
  Added `rfe_case39.s` (byte-comparison only, RFE being privileged and
  illegal under qemu-mips user-mode emulation same as
  `cop0_move_case37_38.s`'s MTC0/MFC0). PASS byte-identical + PASS
  same qemu-mips trap on both sides.
- **Case 47** `sc r,soreg` / **case 48** `ll soreg,r` -- atomic
  store-conditional/load-linked. Required new AST from scratch (no
  existing constructor fit their asymmetric semantics -- SC writes a
  success/failure flag back into its register operand on real
  hardware, not a plain store): added top-level `LL of gen * reg` and
  `SC of reg * gen` to `instr` (`Object_file.version` bumped 13->14),
  new TSC/TLL tokens and lexer keywords, and grammar rules mirroring
  case 7/8's shape.
  ZOREG (offset==0) only -- and here that's not just the usual BIG=0
  dead-fast-path story: goken's optab.c *never declares* a
  large-offset/entity variant for ASC/ALL at all. SC (store) has no
  delay slot; LL (load) gets the same mandatory 1-NOP load-delay-slot
  hazard as case 8/27/36.
  Added `atomic_case47_48.s` (LL kept right before SYSCALL, nothing
  eligible to hoist, byte-identical) and `atomic_case47_48_check.s`
  (the real LL-then-SC protocol, followed by a plain load to confirm
  the value landed, self-checked with BEQ). Both fixtures use the
  stack pointer as the memory operand. PASS byte-identical
  (`atomic_case47_48.s`) + PASS functionally (both).
- **Case 0** pseudo ops -- checked against goken directly (asm.c line
  ~723): this is goken's own pass-1 (size computation) TEXT/GLOBL
  bookkeeping, not a real machine-word-emitting case at all (o1..o7
  are never set to actual output there). Already fully covered by the
  existing, already-tested shared machinery: `Rewritev.ml`'s
  TEXT-prologue rewrite (autosize computation, the leaf/RLINK-save
  logic fixed in the case 26 entry) and `Codegen.default_rules`
  (T.TEXT/T.WORD, same shared path as case 40). Nothing left to
  port.
- **Case 45** `case r` / **case 46** `bcase $con,lbra` -- **confirmed
  dead on this arch, not ported**. The jump-table switch-statement
  pair. Unreachable from `.s` source (no grammar rule in goken's
  `va/a.y`), and goken's own MIPS compiler (`vc/swt.c`) never emits
  it either -- its switch lowering is a binary-search of branches
  instead, unlike 5c/7c/2c. No way to make goken itself exercise
  this, so no way to test a port. Documented in `Codegenv.ml`, right
  before the "System" section; no CASE/BCASE constructor added to
  `Ast_asmv.ml`. This closed out the MIPS backlog.

## hello_libc integration test

Status: **complete**. Same idea as `arm_port.md`'s/`arm64_port.md`'s
own equivalent sections (read `arm_port.md`'s first -- this is the
MIPS sibling, same methodology, run in a later session): beyond the
hand-written `tests/linker/mips_diff/` fixtures above (each one
object file, one `TEXT`, no real linking), `tests/linker/
hello_libc_mips/` stress-tests the whole pipeline against goken's own
real `hello.c` (which calls into a real, reusable `lib_core/libc/
libc.a`), compiled via real `vc -S` for its full transitive libc
dependency closure (36 files), assembled with `ova`, linked with
`ovl`, and run under `qemu-mips`. The fixture is self-contained
(`hello.c`, `closure.tgz`, `test.sh`, `Makefile`, mirroring
`hello_libc_arm/`'s exact structure) and needs no goken checkout to
run day-to-day.

This was the largest single-session gap-closing effort of the whole
hello_libc series -- roughly 20 distinct real gaps closed (vs ARM64's
~9), because `Codegenv.ml`/`Ast_asmv.ml` had much thinner
pre-existing coverage going in: `Move1`'s own byte/halfword forms had
*zero* memory-access codegen at all before this session, only
register-to-register sign/zero-extend, for instance.

**The goken-side assembler bug, found first, same shape as 5c/7c's
already-fixed one.** goken's own `vc -S` (MIPS's C compiler) has the
identical comma-padding `Pconv` artifact `compilers/5c/list.c`/
`compilers/7c/list.c` already needed fixing earlier in this same
session series -- `compilers/vc/list.c`'s own `Pconv` prints a stray
leading/trailing comma next to a `D_NONE` operand (`"RET\t,"`,
`"JAL\t,foo+0(SB)"`), which isn't valid `va` input either. Same fix
applied: strip the dangling comma inside `Pconv` itself, right before
`fmtstrcpy`. Rebuilt cleanly via `mk objtype=boot-gcc install`.

**A second, genuinely new goken `-S`-printing quirk (not
comma-related).** goken's real `Pconv` prints a *negative* "N(PC)"
relative branch offset as its raw 32-bit-*unsigned-wraparound*
decimal value instead of a signed one -- e.g. `"4294967291(PC)"` for
what's really `-5(PC)` (a backward loop branch). This round-trips
fine through real goken's own `va` (its C `int32` arithmetic wraps
the same way), but xix's OCaml `int` is 63-bit, so `!pc + i` without
truncation lands billions of instructions out of range instead of 5
instructions back. Fixed in the **shared**
`assembler/Resolve_labels.ml` (not MIPS-specific -- `Relative i ->
Absolute (!pc + i)` now goes through `Int32.of_int i |>
Int32.to_int` first, unconditionally, since it's a no-op for every
realistic small relative offset), verified zero-regression across
every arch. This is a *third* instance of the "goken's own -S/print
output isn't valid re-assembleable input to itself" family, after the
comma-padding bug and ARM64's `R31`-register-name bug (see
`arm64_port.md`) -- worth specifically checking for on any *future*
arch's own hello_libc effort too.

**~20 real gaps closed getting the 36-file closure to assemble+link**
(none exercised by any `mips_diff/*.s` fixture, all found by feeding
the real closure through `ova`/`ovl` and reading the resulting
error): a real "JAL 0(Rn)" (a call through a function pointer --
confirmed real `va` grammar via `assemblers/va/a.y`'s own `nireg: ireg
| con ireg`, genuinely more permissive than ARM's own `nireg: name |
ireg`; goken's real optab.c row `{ AJAL, C_NONE, C_NONE, C_ZOREG, 18,
4, REGLINK }` gives R31 as the implicit link-register destination);
"$off(Rbase)" as a plain address-of-register-indirect (goken's real
case 3, "mov $soreg,r ==> or/add $i,o,r" -- bypasses `Move2` entirely
since the AST's shared `Ast_asm.ximm`'s own `Address` only wraps
Param/Local/Global, never an arbitrary register+offset); byte/
halfword memory access (`Move1`'s own B_/H_ forms, both the
zero-offset fast path and the REGTMP-materialize expansion for a
nonzero one); the plain 2-operand "NOR $imm,Rd" idiom (MIPS's own way
to spell bitwise NOT when imm=0, confirmed directly against goken:
"NOR $0,R1" on R1=5 gives R1=~5=250 as an exit code); AND/OR/XOR/SGT
with an immediate (both the direct 16-bit-range case, whose own
`opirr_arith_opcode` rows already existed with nothing dispatching to
them, and a REGTMP-materialize fallback for anything bigger -- real
`va`'s own fallback there is a genuine literal pool this port doesn't
implement, so this deliberately diverges, matching goken's own
computed *value* exactly without matching its bytes); real hardware
DIV/DIVU (same "no destination register field, result lands in
HI/LO" shape as the already-wired MUL); the word&harr;float&harr;double
conversion family (MOVWD/MOVDW/MOVWF/MOVFW/MOVDF/MOVFD, a new
`Ast_asmv.fcvt_dir`/`FCvt` constructor -- goken's own real case 33,
sharing ABS_/NEG_'s exact encoding shape via its `span.c` `buildrep()`
mechanism); plain float/double register-to-register moves (also real
case 33, `AMOVF`/`AMOVD`'s own optab rows, reachable through the
*already-existing* `Move2`/`vgen` grammar with no new AST or grammar
needed at all, just a missing dispatch arm); double-precision memory
access (MIPS32 has no native 64-bit FPU load/store at all -- goken's
own real case 27/28 C code splits a double access into two
F__-sized (LWC1/SWC1) halves against a register pair, confirmed
directly: the low address word loads into Fn+1, address+4 into Fn);
and a double float *literal* load (splitting the constant's own raw
IEEE754 bit pattern into two 32-bit halves, MTC1'd separately into
the register pair).

**Two real, confirmed bugs found and fixed along the way** (not new
gaps -- genuine bugs in behavior this port already claimed to
support):

1. **`CMPEQ_`/`CMPGE_`/`CMPGT_`'s first implementation silently
   computed the wrong comparison result.** Reasoned from goken's own
   C source alone (`OP_FRRR(oprrr(p->as), p->from.reg, p->to.reg,
   p->to.reg)`, the exact same shape as every other case-32 op) it
   looked correct -- but a real MIPS FP *compare* instruction has no
   third 5-bit register field at all in that bit position (bits
   [10:6] are the condition-code selector, not `fd`), so reusing the
   generic 3-register `op_frrr` helper unmodified set an extra, wrong
   bit. Caught only by manually decoding real goken's own linked
   instruction words for a hand-written `"CMPEQD F2,F4"` probe and
   finding the raw bytes differed from the naive derivation by
   exactly bit 8 -- reading the C source alone was not enough this
   time, the same durable "verify against the real reference binary"
   lesson `arm64_port.md` already documents, but for a case where the
   derivation *looked* airtight going in.
2. **`"MOVW $sym+N(SB),Rt"` (address-of-global-plus-offset) silently
   computed `sym+0` for any nonzero N.** The *exact same bug shape*
   ARM32's own port already found and fixed (see `arm_port.md`'s own
   hello_libc section) -- the offset parameter was even still named
   `_offsetTODO` in `Codegenv.ml`, OCaml's own "acknowledged but
   unused" marker, just never actually wired up for MIPS. This
   construct never arises from hand-written assembly (you'd just give
   each datum its own zero-offset symbol) -- it only comes from a
   real compiler packing multiple string literals into one shared
   `.string<>`-style blob per C file. Broke `hello.c`'s own real
   `"%d"` formatting (`fmt/dofmt.c`'s shared digit-table string) --
   the closure *linked and ran* with this bug present, printing
   plausible-but-wrong ASCII (`"hello from libc.a: i + i = >"`
   instead of `"...2 + 2 = 4"`) rather than crashing, which is
   exactly why it took a real end-to-end run (not just a link success
   check) to catch.

**A real encoding-mechanics gotcha in this port's own `Bits.t` sanity
checker**, hit fixing BFPT/BFPF ("branch if the FP condition flag is
true/false" -- confirmed *real* `va` grammar, `LTYPEG` in
`assemblers/va/a.y`, no register operand at all, set by a preceding
CMPEQ_/CMPGE_/CMPGT_): goken's own base opcode is one flat literal
(`SP(2,1)|(257<<16)` for BFPT), but placing that as a single
`[(257,16)]` `Bits.t` entry passed OCaml type-checking yet failed at
*link time* ("value 257 overflow outside its space (21-16)") --
`Bits.sanity_check_32` infers each field's own bit-width from the gap
to its *nearest neighboring declared position* in the list, not from
real bit 32, and a neighboring caller (`op_irr_no_r3`) always places
its own field at bit 21, boxing in bit 16's own inferred width to
just 5 bits. Took two attempts to get the re-split right (first tried
a `bcond`-style two-field split at bit19/bit16, which just moved the
overflow one field over) -- the working fix decomposes 257/256's own
actual *set bits* (bit 24 and bit 16) into two genuinely
non-overlapping fields rather than guessing at a generic split.

**Fixture discipline**: `mips_diff/global_offset_case19.s`
(byte-identical, the address-of-global bug above) plus three
`_check.s` fixtures (functional-only, matching this port's own
already-established `_check` naming convention -- see
`tests/linker/README.md`) for JAL-indirect
(`indirect_jump_case18_check.s`), address-of-indirect
(`addr_of_indirect_case3_check.s`), and BFPT/BFPF+CMPxx together
(`bfpt_bfpf_case6_check.s`, the compare bug above, found via this
exact probe). A first BFPT/BFPF fixture attempt used a bare `"MOVD
$2.0,Fn"` float literal and hit a *separate*, unexplained goken
behavior (its own linked binary balloons to 4104 bytes, suggesting
some of its 8 "chipfloat" values route through a genuine literal pool
this port doesn't replicate) -- sidestepped by building the two equal
doubles via `MOVWD` (int-to-double conversion) instead, which is what
actually isolated the real compare bug.

## Open issues

- **GLOBL-without-DATA total file size mismatch**: flagged while
  testing case 27/28's Entity variant. A GLOBL'd symbol with no
  corresponding DATA statement produced very different total
  executable sizes between goken (~4300 bytes) and xix (~350-4100
  bytes across two attempts) -- likely a BSS/data-segment layout
  difference unrelated to case 27/28 itself (every *working*
  GLOBL+DATA fixture, e.g. `kitchen_sink.s`, matches fine), but not
  root-caused. Check `Layoutv.ml`/`Datagen.ml`'s BSS handling against
  goken's `dodata()` before trusting a GLOBL-only symbol's layout on
  MIPS.

## Post-completion fix (found while porting RISC-V)

**A real, previously-latent bug in `Rewritev.ml`'s RET expansion for
a leaf procedure that still declares a nonzero frame** ("leaf with
locals" -- no calls, but has locals). Found while reading goken's
`il/noop.c` for RISC-V's own 3-way leaf/frame logic and checking
whether MIPS's `vl/noop.c` has the same shape (it does, confirmed by
reading its `ATEXT`/`ARET` cases directly). Before the fix,
`Rewritev.ml` always expanded RET as "load RLINK from 0(SP); restore
SP; jump to it" regardless of leaf-ness, but the matching prologue
only ever *wrote* RLINK to 0(SP) for a non-leaf TEXT -- for a
leaf-with-locals procedure, 0(SP) was never initialized, so the
"restored" return address was garbage, jumped to unconditionally.
Confirmed as a real, live bug (not just theoretical): reverting the
fix and re-running `tests/linker/mips_diff/leaf_locals_ret_check.s`
segfaults under `qemu-mips`. Fixed by threading `(autosize,
needs_link_save)` through `Rewritev.ml`'s step2 (same shape as
`Rewritei.ml`'s own `frame : (int * bool) option`, RISC-V), so RET
now correctly takes the no-save/no-restore "case 2" shape for a leaf
with locals. Not byte-identical against goken (`_check` fixture, not
in `test-mips.sh`'s `CASES` list): goken's `sched.c` hoists the
epilogue's SP-restore into the RET's own delay slot, the same
documented, deliberately-not-replicated scheduler gap as everywhere
else in this port -- confirmed this hoisting happens for *both* the
leaf-with-locals and non-leaf RET shapes, not something new this fix
introduced.
