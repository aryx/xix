# Porting the ARM64 toolchain (o7a/o7l) against goken, byte-equal

Status: **complete** for the scoped feature set. `o7a`/`o7l` exist,
assemble+link real `.s` fixtures to output byte-identical to goken's
real `7a`/`7l`, and are verified under `qemu-aarch64`. Covered:
Arith/Shift/Cmp/ArithMul (both 64-bit and `*W`-suffixed 32-bit-view
forms), Move (register-register, sized byte/halfword/word/doubleword
memory access via both plain register-base and the SB-relative fast
path, pre/post-index writeback, `MOV $con,R` via goken's real
constant-classification chain, address-of-global and large constants
via the literal pool), B/BL/Bxx/CBxx/TBxx branches, RET (the real
hardware instruction) and RETURN (the compiler pseudo-op, with full
leaf/frame-size-driven prologue/epilogue synthesis), SVC, floating
point (FArith/FCmp dyadic arith+compare, FMOVS/FMOVD/SCVTF*/FCVTZS*
move+convert), DMB/DSB/ISB barriers, the conditional-select family
(CSEL/CSINC/CSINV/CSNEG/CSET/CSETM, including the CINC/CINV/CNEG
alias spellings), the X-width exclusive-monitor atomic pair
(LDXR/LDAXR/STXR/STLXR), and AND/ORR/EOR's own bitmask-immediate form
(scoped to e=64 patterns, see "Open issues"; the `*W`-suffixed 32-bit
forms go through a different, always-correct REGTMP substitute
instead of goken's own confirmed-buggy narrow encoder -- see "hello_libc
integration test"). Deliberately dead or out of scope: see
"Investigated and skipped" and "Open issues" below. The real, full
`hello.c`+`lib_core/libc` closure additionally links and runs
correctly, natively and under `qemu-aarch64` -- see "hello_libc
integration test" below for the several real gaps (including a
native-execution-only segfault) only that scale of test could find.

## Goal

Same methodology as `arm_port.md` (ARM32), `mips_port.md` (MIPS), and
`riscv_port.md` (RISC-V): assemble+link the same `.s`
with both goken's 7a/7l (Plan 9 ARM64 assembler/linker, `~/goken`)
and xix's o7a/o7l, and require the final executables to be
byte-identical. Scope is ARM64 assembler + linker only, permanently
-- the compiler (7c) is out of scope for the exact same reason as
every other arch (occ's plan is c--'s backend, not per-arch goken
compiler parity).

Read `arm_port.md` first for the harness shape, syncweb rules,
where-changes-land guidance, and goken-flag conventions shared across
every arch in this effort -- this doc only calls out what's
ARM64-specific.

## The one load-bearing difference from every arch ported so far

MIPS and RISC-V32/64 both started from an existing, if partial, xix
side. ARM64 had neither: before this port, `assembler/objects/TODO/
Ast_asm7.ml` was a 30-line unwired stub and there was no
`Parser_asm7.mly`, `Codegen7.ml`, `o7a`, or `o7l` at all -- this port
needed a full assembler (grammar, lexer, AST, `Parse_asm7.ml`/
`Object_file` plumbing) *and* the linker's codegen, in that order,
built from scratch in one continuous session (with two review
checkpoints: the initial basic structure, and this final writeup).

`lib_core/commons/Arch.ml` already had `Arm64` as a first-class
variant (`Endian.Little`, `Arch64`, letter `'7'`) -- the one piece of
cross-cutting plumbing that needed real thought for MIPS (endianness)
was already correctly wired here.

## Real findings, gotchas, and design decisions

**Register 31 needed no special AST handling.** AArch64 reuses
register slot 31 for either the stack pointer or the zero register,
disambiguated by which instruction/operand *position* it appears in.
This turned out to need zero special-casing: goken's own grammar
lexes both "ZR" and "RSP" to the exact same `D_REG`/register-31 node
(confirmed reading `lex.c`), so both spellings just map to `R 31`
here too, and `Codegen7.ml`'s per-instruction encoding logic (e.g.
`gmov_reg_reg`'s SP-aware ORR-vs-ADD branch) decides what register 31
means from context, exactly like the real hardware does. Plan9's
unrelated bare "SP" pseudo-token (local-variable addressing, shared
with every arch) is a completely different thing, not wired as a
register operand here.

**goken's real `MOV $con,R` constant classification is a priority
chain, not a simple "does it fit" check.** Confirmed empirically:
`$7` (a bitmask-shaped value) assembles to a literal-pool load, while
`$42` (equally small, not bitmask-shaped) assembles to a direct
`MOVZ`, even though both trivially fit one 16-bit lane. `span.c`'s
`aclass()`/`cmp()` chase `isaddcon`/`isbitcon`/`movcon` in a specific
order: `v=0` &rarr; MOVZ #0; else if `isaddcon(v)` (fits ADD/SUB's own
12-bit-optionally-shifted immediate) &rarr; pool if also `isbitcon`,
else MOVZ if `v<=0xFFF`, else pool; else `movcon(v)`/`movcon(~v)`
&rarr; MOVZ/MOVN at that lane; else &rarr; pool. Fully ported in
`Codegen7.ml`'s `isaddcon`/`isbitcon`/`movcon`/`move_immediate_encoding`,
each independently verified against real goken output before being
trusted.

**Address-of-global does NOT always go through the literal pool** --
a real bug this port shipped and then caught and fixed. `span.c`'s
`aclass()` takes a direct `ADD $offset,RSB,Rd` fast path (goken's
`C_AECON`, case 4) whenever the resolved SB-relative offset is BOTH
nonzero and fits ADD's addcon range; the pool is only the fallback
for offset=0 or an offset too big for addcon. The first literal-pool
fixture happened to place its lone global at data-offset 0, so it
only ever exercised the pool path; the bug only surfaced once a
second global at a nonzero offset was added and byte-diffed against
goken (see `tests/linker/arm64_diff/sized_move.s`).

**REGSB (X28) has no bias at all**, unlike ARM32's BIG=4092 or
RISC-V's BIG=2048 -- `setSB` is defined at plain data-offset 0
(`xdefine("setSB", SDATA, 0L)` in `pass.c`), confirmed empirically.
Every SB-relative fixture needs an explicit `MOV $setSB(SB), R28`
bootstrap first, same convention as MIPS's `MOVW $setR30(SB),R30` /
RISC-V's `MOVW $setSB(SB),R3`.

**Branch/CBZ/Bcc/TBZ immediates are word-scaled**
(`(target-pc)>>2`), with no ARM32-style "PC = instr+8" pipeline bias
-- AArch64 branch fields are always relative to the branch
instruction's own address. Missing the `>>2` was a real bug caught
by byte-diffing (every non-adjacent branch target came out 4x too
far).

**The literal pool's entries are full 8-byte DWORDs**, not 4-byte
words like ARM32's -- AArch64's pool-fed loads are always 64-bit
LDRs, and goken never routes a `*W`-sized move through the pool.
Each pool entry is spliced in `Layout7.ml` as a chained low/high
`T.WORD` node pair (not a single node), and goken 8-byte-aligns the
pool's start and never deduplicates identical values (confirmed:
two loads of the same address get two separate, separately-aligned
8-byte slots) -- a pre-flush pad node is needed whenever the natural
post-guard-branch position isn't already 8-aligned. Verified with a
fixture chaining two pool entries in one flush
(`tests/linker/arm64_diff/global_addr.s`).

**goken has two distinct return-shaped mnemonics.** The real
hardware `RET [Rn]` (a plain instruction, no side effects) and a
separate compiler-facing `RETURN` pseudo-op that `noop.c` expands
based on leaf/frame analysis (leaf = no `BL` anywhere in the
procedure, unrelated to whether the body references R30 directly).
`Rewrite7.ml` ports `noop.c`'s algorithm directly: PCSZ=8,
STACKALIGN=16, the same 3-case leaf/frame shape already familiar
from `Rewrite{5,v,i}.ml`, just with ARM64's own SUB/ADD SP
adjustment and a pre/post-index STR/LDR for the link-register
save/restore. This needed new `PreIndex`/`PostIndex` addressing
(goken's D_XPRE/D_XPOST, `"-16(RSP)!"`/`"(RSP)16!"`) and a new shared
`TBANG` (`"!"`) token.

**The `*W`-suffixed 32-bit-view forms are sibling opcode
constructors, not a width field.** Confirmed against `asmout.c`'s
`oprrr()`/`opirr()` tables: every `*W` row (ADDW/LSLW/CMPW/MULW/...)
is byte-for-byte identical to its 64-bit counterpart with only the
"sf" bit (bit 31) cleared (and, for the bitfield-move-encoded
immediate-shift forms, the N bit also cleared and immr/imms computed
mod 32 instead of 64) -- so `ADDW`, `LSLW`, etc. are modeled as
sibling constructors of `ADD`, `LSL`, etc. rather than threading a
separate width field through the AST/grammar.

**FMOVS/FMOVD/SCVTF\*/FCVTZS\* are dispatched as `Move`, not their
own constructor.** goken routes all of them through the exact same
generic "gen,gen" LTYPE3 grammar production as ordinary MOV, so they
extend the existing `Move`/`gen`/`move_size` machinery (a new
`GFReg` operand, float-specific `move_size` tags) instead -- this let
float&harr;memory and the SB-relative fast path fall out of the
already-generalized sized-move code almost for free. `FArith`/`FCmp`
(dyadic arith/compare) DO get their own constructors, since they're
genuinely 3-operand arithmetic (LTYPEK/LTYPEL), not data movement.
goken's own float-*immediate* operand support is dead code in the
reference implementation itself (`if(rf<0||1) diag(...)` in
`asmout.c`'s case 54 always errors) -- this port carries no
float-immediate operand either; every float value in a fixture comes
from an int-&gt;float conversion (`SCVTFD`) instead.

**CINC/CINV/CNEG share CSINC/CSINV/CSNEG's own opcode identity.**
`asmout.c`'s `oprrr()` merges each alias pair into one shared base
opcode (`case ACINC: case ACSINC: return ...`), and goken's grammar
is fully permissive about which spelling gets 2 vs 3 explicit
registers -- the register *count*, not the mnemonic name, decides
whether case 18 inverts the condition and reuses the single source
register as both operands. So this port needs only one opcode
constructor per shared row (`CSINC`, not separate `CINC`/`CSINC`
constructors), with the alias mnemonics mapped onto it in
`Parse_asm7.ml`'s keyword table.

**STXR/STLXR's real operand order was confirmed empirically, not
assumed.** goken's grammar textually reads "STXR Rt,(Rn),Rs" (value,
address, status) -- verified by assembling a concrete instruction
with real goken and disassembling it (`STXR R1,(R2),R3` &rarr;
`stxr w3, x1, [x2]`), not derived from the C source alone, given
this family's documented bug history elsewhere.

**A real bug found in goken's own bitmask-immediate encoding for
sub-64-bit element sizes.** While implementing AND/ORR/EOR's own
immediate form, direct testing against real goken turned up a case
where it emits demonstrably wrong bytes: `AND $0x0202020202020202,
R1,R2` (a clean e=8 pattern, one bit set per byte) assembles to an
instruction that decodes back to `0x0200000002000000` -- a different
value entirely (confirmed by manually decoding the raw instruction
word's N/immr/imms fields against the standard ARM64 algorithm, not
just trusting `objdump`'s summary line). `$0x0101010101010101` (also
e=8) round-trips as only `0x0000000100000001` (as if e=32). This
looks like a real bug in goken's own `findmask64()`/`maxstr1()` for
element sizes below 32/64 bits, not something worth differentially
testing against (there's no confidence it's even a stable, well-
defined "bug" reproducible the same way across all inputs). This
port's own `bitmask_immediate_encoding` is therefore scoped to
**only** the e=64 case (a single contiguous run of 1-bits, any
rotation, never a smaller replicated element) -- verified extensively
for this subset (values 1/3/7/0xFF/0xFFFF/0x1FE, including a
genuinely rotated pattern, all byte-identical and numerically
correct against goken). Values needing e&lt;64 replication are simply
not classified as bitcon here and error loudly rather than emit
anything.

**A real, reproducible dune build-staleness gotcha specific to this
port.** `rm -f _build/default/bin_dune/o7a _build/default/bin_dune/
o7l && dune build` (the fix that's always worked for every other arch
in this project) was NOT sufficient here on multiple occasions -- a
confirmed-present source edit still didn't show up in
`_build/default/bin_dune/o7l`'s actual behavior until `dune build
--force` was used, and even that wasn't always enough: at least once
a stale binary's `strings -a` output still showed an old debug
string already removed from the source, requiring `rm -rf
~/.cache/dune/db && dune build --force` (clearing dune's shared build
cache) to get a genuinely fresh binary. When in doubt, `strings -a`
the binary for an expected/removed marker or compare mtimes against
the source before trusting a build's output.

## Investigated and skipped (confirmed dead in goken itself)

**Load/store pair (LDP/STP, goken's "MOVP"/"MOVPQ"/"MOVPD" family).**
Declared in the grammar/lexer/opcode-enum, but `asmout.c`/`optab.c`
have ZERO rows or cases for `AMOVP` at all -- confirmed empirically:
every operand ordering tried (`MOVP R1,R2,0(R3)`, `MOVP 0(R3),R2,R1`,
`MOVPQ`/`MOVPD` variants) fails to LINK with real goken ("illegal
combination MOVP ..."), regardless of spelling. This is genuinely
dead/unimplemented functionality in the reference implementation
itself, not a flag-gated or dlm-only feature -- there is no ground
truth to differentially test against, so implementing it here would
mean inventing an encoding with nothing to verify it against. Same
treatment as `arm_port.md`'s case 62/63 (CASE/BCASE) writeup.

**Bare NOP.** A genuinely dead pseudo-op in goken's own reference
implementation -- `noop.c`'s `case ANOP: q->link = q1; continue;`
deletes it from the instruction stream entirely before it ever
reaches `asmout.c`, despite `asmout.c` having a (dead, unreachable)
case for it. There is no real "NOP" byte sequence to differentially
test against.

## Deliberately out of scope (not investigated as dead, just not attempted)

- **HINT** (`$imm` form) -- same LDMB/opirr shape as DMB/DSB/ISB
  (which ARE implemented), just not wired since it's rarely used
  directly outside of NOP (which is itself dead, see above).
- **SYS/SYSL/MRS/MSR** -- real system-register selectors, a
  separate and more involved family than the barriers.
- **SIMD/vector registers and instructions** -- an entirely new
  register file and operand shape with no ARM32/MIPS/RISC-V
  analogue in this codebase; occ's future c---backend plan makes
  this unlikely to matter for compiler parity any time soon.
- **Bitfield instructions as their own mnemonics** (BFM/BFI/UBFM/
  EXTR) -- LSL/LSR/ASR/ROR *by immediate* already use this same
  bitfield-move encoding family under the hood (case 8), so the
  common case is covered; the raw mnemonics themselves aren't wired.
- **Sub-word (B/H/W) atomics, and plain (non-exclusive) LDAR/STLR**
  -- LDAR/STLR are a genuinely different grammar shape (LTYPE3's
  plain "gen,gen", not LDXR/STXR's own 2-or-3-operand shape) from
  the exclusive-monitor pair this port covers.
- **FABSS/FABSD/FNEGS/FNEGD/FSQRTS/FSQRTD/FCVTSD/FCVTDS** -- same
  FPOP1S shape as FMOVS/FMOVD (already implemented), trivial to add
  later, just not wired as their own mnemonics yet.
- **The signaling FCMPES/FCMPED compare variants.**
- **ADR/ADRP** -- AArch64's normal PC-relative address-load
  mechanism in real-world code, but this port's literal pool already
  covers address-of-global correctly, so there was no forcing
  function to implement these too. (A real ADR *is* now wired, but
  only as the fixed `ADR X17,#16` CASE/BCASE needs internally -- see
  "hello_libc integration test" -- not as a general-purpose mnemonic.)
- **B/H/W pre/post-index** writeback (only X_'s pre/post-index is
  wired, needed for `Rewrite7.ml`'s own RETURN expansion).
- **CCMN/CCMP** (conditional compare) and **extended-register /
  shifted-register addressing modes** (UXTB/UXTH/UXTW/UXTX/SXTB/...).

## hello_libc integration test

Status: **complete**. Same idea as `arm_port.md`'s own equivalent
section (read that one first -- this is the ARM64 sibling, same
methodology, run in a later session): beyond the hand-written
`tests/linker/arm64_diff/` fixtures above (each one object file, one
`TEXT`, no real linking), `tests/linker/hello_libc_arm64/`
stress-tests the whole pipeline against goken's own real `hello.c`
(which calls into a real, reusable `lib_core/libc/libc.a`), compiled
via real `7c -S` for its full transitive libc dependency closure (35
files -- one more than ARM32's 34, but *without* `port/vlrt.c`, since
ARM64 has native 64-bit registers/arithmetic and doesn't need
software vlong helpers ARM32's soft-64-bit division does), assembled
with `o7a`, linked with `o7l`, and run **both under `qemu-aarch64`
and natively** (this development host is itself aarch64). The
fixture is self-contained (`hello.c`, `closure.tgz`, `test.sh`,
`Makefile`, mirroring `hello_libc_arm/`'s exact structure) and needs
no goken checkout to run day-to-day.

**~9 real gaps closed getting the 35-file closure to assemble+link**
(none exercised by any `arm64_diff/*.s` fixture, all found by feeding
the real closure through `o7a`/`o7l` and reading the resulting error):

- **CASE/BCASE turned out to be *real* 7a grammar** -- a genuinely
  different finding from ARM32, where the equivalent construct is
  confirmed dead in goken's own real `5a` (see `arm_port.md`'s case
  62/63 writeup). ARM64's real `assemblers/7a/lex.c` has real
  `ACASE`/`ABCASE` lexer rows, so this port's own `CaseJump`/`BCase`
  constructors needed a *real*, byte-matching implementation (the
  full ADR+LDRSW+ADD+BR expansion, a relative-offset table anchored
  at `case_pc+16`), not the "xix-only approximation, no real grammar
  to match" writeup this doc's own `CASE`/`BCASE` history originally
  assumed by analogy with ARM32 -- corrected in `Ast_asm5.ml`'s own
  comment too, which had over-generalized the ARM32 finding to
  "5a/7a" collectively.
- **NEG/MVN/SXTW/UXTW**, **REM/UREM** (goken's own real 2-instruction
  SDIV+MSUB synthesis -- AArch64 has no hardware remainder
  instruction), real hardware **SDIV/UDIV** (case 1's plain
  "op Rm,[Rn,]Rd" shape, nothing pseudo-op about division itself,
  only *remainder* needs synthesis), and the **UCVTF/FCVTZU/SCVTF/
  FCVTZS W-register quadrant** (completing goken's own 16-combination
  FPCVTI family's D-precision&harr;W-reg corner).
- **Float literal-pool loads** (`FMOVD $con,Fd`) -- goken's own real
  chipfloat-immediate mechanism is dead code in the reference
  implementation itself (same as ARM32's FPA chipfloat, see that
  doc's own float-immediate writeup), so real `7c`-compiled code
  needing an arbitrary double constant routes through the pool
  instead; this needed a new `gload_from_pool_f` (the "V" float-bit
  set on the same PC-relative literal-load mechanism `MOV $con,R`
  already used) to match.
- **The unscaled 9-bit-signed ("LDUR"/"STUR"-style) addressing
  form** -- goken's own real fallback (case 20/21's `if(v<0)`
  branch) whenever an offset doesn't divide evenly by the access
  size, reusing the exact same `(sz,v,opc)` table the scaled
  12-bit form already needed (`LDSTR9S`'s bit layout is `LDSTR12U`'s
  with just bit 24 cleared instead of set).
- **ADD/SUB negative-immediate flipping** -- ported directly from
  goken's real `linkers/7l/obj.c` (`case ASUB: if(isnegoff(p)) {
  p->from.offset = -p->from.offset; p->as = AADD; }` and the AADD
  mirror, plus the ADDW/SUBW siblings), exactly the same mechanism
  ARM32's own `Rewrite5.ml` already needed -- confirmed real for
  ARM64 too by directly reading `obj.c`, not assumed by analogy.
  Found via real `fmt/utf`'s own `"ADDW $-1,R12,R12"` loop decrement.
- **ANDW/ORRW/EORW's 32-bit bitmask immediate** -- see "Open issues"
  above for the real encoder's own confirmed bug; every W-suffixed
  immediate goes through a REGTMP-materialize-then-register-op
  substitute instead of attempting goken's own narrow-element-size
  encoding at all.
- **Register-relative `Indirect` load/store's 3-tier offset
  handling** (scaled-12-bit / unscaled-9-bit / a new
  `gindirect_huge` REGTMP+ADD "materialize the real address, then
  zero-offset access" fallback for anything bigger) -- the plain
  register-base case had only ever gotten the scaled-12-bit fast
  path before, since no small fixture happened to need a negative or
  huge register-relative offset.
- **ADD/SUB/ADDW/SUBW/CMP/CMPW immediates too big for the real
  "addcon" shape** (12 bits, optionally shifted left 12) -- goken's
  own real fallback there is a genuine "$lcon" extended-register
  literal-pool mechanism this port doesn't implement; same REGTMP-
  materialize substitute as the bitmask-immediate case above.
- **A real bug in the *shared* (arch-independent) `Codegen.ml`'s
  `WORD` case**: `Ast_asm.Float` unconditionally `raise Todo`'d
  (nothing had ever routed a float value through the shared
  literal-pool WORD path before), and `Layout7.ml`'s own high-word
  splicing hardcoded the high 32 bits of every float literal to 0 --
  silently truncating any double whose upper half wasn't zero. Fixed
  by actually splitting the constant's raw IEEE754 bit pattern
  (`Int64.bits_of_float`) into its low/high 32-bit halves.

**The one that took actual native execution -- not just linking --
to catch.** The real `(FP)`/`(SP)` named-local/param offset formula
was **silently wrong for any non-trivial frame**. (Naming note: this
port's own `A.entity` constructors are swapped from their shared doc
comment across every arch -- ARM64's grammar, like ARM32's own, binds
the real `(FP)` token to `A.Local` and the real `(SP)` token to
`A.Param`, so a real "(FP)" caller-parameter reference surfaces here
as `A.Local` and a real "(SP)" local-variable reference as `A.Param`
-- see `Codegen7.ml`'s own `local_param_offset` comment for the full
explanation.) `Rewrite7.ml`'s own TEXT-node mutation
stores `autosize - pcsz` (pcsz=8) as the codegen-visible declared
size whenever a real frame exists, but the formula used that value
as if it were the true frame size directly -- correct only for the
one case where they coincide (a leaf function with declared size
exactly 0). No `arm64_diff/*.s` fixture exercised `A.Local`/`A.Param`
at all (the exact same blind spot ARM32's own port hit once, see
`Codegen5.ml`'s own "+4" comment history) -- so this slipped past
every regression fixture *and* past the full closure successfully
*linking*, surfacing only as a NULL-pointer segfault at actual native
runtime (`dofmt` reading a garbage `fmt` format-string pointer, 8
bytes short of where its caller `vfprint` actually wrote it).

Root-caused by disassembling the raw failure directly (gdb can't load
this port's own minimal-ELF output any more than ARM32's -- see
`arm_port.md`'s own gdbstub-over-qemu technique for that arch; ARM64
instead used `strace -f` first to confirm a very-early NULL deref,
then `objdump -D -b binary -m aarch64 --adjust-vma=<vaddr>` on the
raw `.text` bytes extracted via `dd`+`readelf -l`'s own program
headers, and, once a real gdb session was worth the setup, a small
Python patch zeroing `e_shoff`/`e_shnum`/`e_shstrndx` in a scratch
copy so BFD/gdb would load it at all), then independently confirmed
against **real goken** with two tiny hand-written probes rather than
continued disassembly reading: a leaf callee's own `arg+0(FP)`
compared against a raw `RSP`-relative store at the same conceptual
slot in its caller, and a non-leaf caller's own named local
`x-8(SP)` compared the same way -- each probe swept candidate offsets
until the real-goken-linked binary's own exit code confirmed which
one was actually correct. Fixed by factoring the formula into one
`local_param_offset` helper (`Codegen7.ml`) that reconstructs the
*true* frame size before applying either offset; new
`arm64_diff/fp_offset.s`/`sp_offset.s` fixtures close the blind spot
for good -- both byte-identical against real goken, not just
same-exit-code, unlike most of this section's other new fixtures (see
below).

**A confirmed real goken self-consistency bug found along the way**
(not a xix issue): goken's own real `7c -S` output for part of this
closure uses raw `R31` as a plain register token (e.g. `MOV
R9,16(R31)`), but goken's own real `7a` lexer only recognizes
`RSP`/`ZR` for register 31, not the generic `R31` spelling --
confirmed directly (`7a` itself rejects it: `syntax error, last name:
R31`) on multiple real closure files. Because of this, goken's own
real `7a` can only assemble 13 of this closure's own 35
compiler-generated units; xix's own `o7a` is more lenient (accepts
`R31` as a plain register reference) and is what lets the whole
closure link at all -- exactly why `hello_libc_arm64/test.sh` checks
*correct behavior*, not byte-parity, against goken (same category as
ARM32's own `port/vlrt.c` finding).

**Fixture discipline**: every gap found chasing the closure got its
own small `arm64_diff/*.s` regression fixture too (`bitmask_w32.s`,
`indirect_offset.s`, `arith_huge_imm.s`, `float_pool.s`, plus
`fp_offset.s`/`sp_offset.s` above), in addition to the large
integration test itself -- most of these are deliberately NOT
byte-identical against goken (either a genuine xix-only REGTMP-
substitute deviation, or real confirmed goken runtime crashes:
goken's own huge-offset `omovlit` fallback SIGSEGVs for a negative
register-relative `Indirect` offset, and its chipfloat mechanism
SIGSEGVs for any non-magic float immediate, both reproduced directly
against real goken/`qemu-aarch64`), documented per-fixture rather
than assumed. The general principle: a large real-program integration
test and small hand-written differential fixtures are complementary,
not redundant -- the large test finds *that* something's wrong at a
scale/convention small fixtures can't reach, but a small fixture is
what actually pins a fix down byte-for-byte (where that's even
possible) and prevents the exact same regression next time.

**Environment gotcha, specific to this session**: a real, reproducible
staleness bug in `_build/default/bin_dune/` (dune's own snapshot copy
of the repo-root `bin_dune` symlink) nearly hid the `local_param_offset`
fix above -- confirmed via md5sum that `_build/default/bin_dune/o7l`
can still be the *previous* build's bytes even right after a clean
`dune build --force`, while `_build/default/linker/Main.exe` (and the
repo-root `bin_dune/o7l` symlink, one hop from it) are both already
the new one. Fixed by pointing `scripts/diff-arm64.sh` at the
repo-root `bin_dune/o7a`/`o7l` symlinks instead -- see this doc's
"Real findings" section for the earlier, less-specific version of
this same class of gotcha, and `arm_port.md`'s own analogous
"install-copies of one shared Main.exe" writeup.

## Open issues

- **AND/ORR/EOR's real bitmask-immediate *encoding* is scoped to
  e=64 only** (see the "real findings" section above) -- a value
  needing e&lt;64 replication (e.g. a clean repeating byte pattern)
  still errors loudly rather than encode via the real single-
  instruction form, even though such values ARE valid AArch64
  bitmask immediates in principle. Revisiting the *real encoding*
  needs either resolving the goken bug/behavior more precisely (so
  there's a real ground truth to match) or a deliberate decision to
  diverge from goken and implement the mathematically-correct
  encoding instead (a real methodology exception, not attempted
  here). This is no longer a *functional* gap, though: the
  `*W`-suffixed 32-bit forms (which inherently always need e&le;32,
  so this exact scope limit would otherwise block every single one of
  them) go through a working, always-correct xix-only substitute
  instead -- REGTMP-materialize the immediate, then the plain
  register-register AND/ORR/EOR form -- found real and necessary
  stress-testing `hello_libc` (see "hello_libc integration test"
  below). Only the bare 64-bit AND/ORR/EOR immediate with an
  e&lt;64-only-representable value remains a hard error.
- **Literal-pool value deduplication** -- goken's `addpool()`-
  equivalent reuses an existing pool word when a new entry's
  constant/address exactly matches one already pending, instead of
  adding a duplicate; NOT ported here either (same known gap as
  ARM32's own `arm_port.md` "Open issues" entry). Low urgency: no
  current fixture repeats an identical large constant/address.
- **No mid-function literal-pool flush** -- only "flush at the true
  end of the program" is implemented (same scope-limit as ARM32's
  own Layout5.ml), so a real program with pool entries spanning more
  than the branch-immediate's reach from their use site isn't
  handled. Not hit by any current fixture.
