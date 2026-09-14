# Porting the RISC-V toolchain (oia/oil, oja/ojl) against goken, byte-equal

Status: **complete**. Every `linkers/il/asm.c` case (0-26) that has a
reachable, real concrete syntax in goken is either ported, confirmed
to need no new code, or confirmed dead/unreachable in goken itself
(cases 19 and 21). Case 18 (a far-branch fallback) is guarded against
silently-wrong bytes but deliberately deferred -- see "Open issues"
at the end for that and everything else still genuinely open.

## Goal

Same methodology as `arm_port.md` (ARM32), `mips_port.md` (MIPS), and
`arm64_port.md` (ARM64): assemble+link the same `.s` with both
goken's ia/il (RV32) and ja/jl (RV64) and xix's own oia/oil and
oja/ojl, and require the final executables to be byte-identical.
Scope is RISC-V assembler + linker only, permanently -- the compiler
stays out of it for the same reason as every other arch.

Read `arm_port.md` first for the harness shape, syncweb rules,
where-changes-land guidance, and goken-flag conventions shared across
this whole effort; this doc only calls out what's RISC-V-specific.
`Codegeni.ml`/`Rewritei.ml`/`Layouti.ml`/`Typesi.ml` are shared
verbatim between RV32 (`oil`) and RV64 (`ojl`) -- an `is_64` flag
threaded through both distinguishes the few places that genuinely
differ (pointer/RLINK width, the AUIPC-vs-LUI address story). Every
fixture in `tests/linker/riscv_diff/` has an identical
`tests/linker/riscv64_diff/` copy for this reason, verified on both.

## The one load-bearing difference from ARM/MIPS: starting from nothing

Unlike ARM/MIPS (fixing bugs in an existing port), RISC-V's *linker*
didn't exist in xix at all before this port -- only the assembler
(`oia`) had been started, by the project owner, before any of this
differential-testing work began. Built from scratch, mirroring the
ARM/MIPS module split exactly (`Typesi.ml`, `Layouti.ml`,
`Rewritei.ml`, `Codegeni.ml`, `link7`-style CLI wiring, `Object_file`/
`Elf.ml` plumbing). `Layouti.ml` is simpler than ARM32/MIPS's own:
RISC-V never needs a literal pool at all -- large constants always
materialize inline via LUI(+ADDI), so there's no pool/splicing
bookkeeping.

A real, blocking bug was found in the *assembler* before any linker
work could even be tested: `Parse_asmi.ml`'s entire keyword table was
a copy-pasted-then-commented-out ARM template, never adapted --
`hello_linux.s` wouldn't even tokenize ("Syntax error, last name:
MOVW"). Fixed by wiring up exactly what was needed at the time
("MOVW"/"ECALL"); everything else (arithmetic, branches, JAL, byte/
half moves, float conversion) was real backlog, closed out case by
case over the rest of this port rather than guessed at up front.

## Real findings, gotchas, and design decisions

**goken's `il` compresses instructions (RVC) by default** --
`linkers/il/compress.c` applies RISC-V's "C" extension (16-bit
encodings for eligible instructions) unconditionally unless `-c` is
passed. Confirmed by disassembling goken's default output and hitting
16-bit opcodes a plain-RV32 disassembler couldn't decode. Deciding
what's losslessly shrinkable, then re-doing layout since it changes
instruction sizes/addresses, is a substantial separate feature --
this port never emits compressed instructions at all; the harness
always passes goken's own `-c` so the comparison stays apples to
apples (qemu handles plain 32-bit-instruction binaries fine either
way).

**The address-of-global (RSB/BIG) story is closer to ARM32 than
MIPS.** RSB (aka SB, aka `gp`/x3) is set up via `MOVW $setSB(SB),
RSB`. `BIG = 2048` (unlike MIPS's dead `BIG = 0`) is a genuinely
*live* value, and not a coincidence: 2048 is exactly the magnitude of
a signed 12-bit immediate's range, so "does the SB-relative offset
fit BIG" and "does it fit ADDI's own immediate field" are the same
check (`fits_addi_imm`) -- genuinely simpler than ARM32's own
bit-rotation search (`immrot`), while still being a live fast path
(unlike MIPS). `hello_linux.s` itself exercises both the fast path
(`MOVW $msg(SB),R11`) and the slow path (`MOVW $setSB(SB),RSB`,
forced there by an explicit `!= 0` exclusion avoiding a circular ADDI
when *defining* `setSB` itself, not by a range/encoding failure the
way ARM32's own `setR12` case happened to work out).

**RISC-V has no branch-delay slots** (a real MIPS-specific
complication that simply doesn't apply here) -- jumps/branches take
effect immediately, no slot to fill, no goken `sched.c`-style hoisting
to worry about replicating. `Rewritei.ml`'s RET/JMP expansion is
correspondingly simpler than `Rewritev.ml`'s own delay-slot-aware one.

**The leaf/frame logic (RET expansion) is genuinely 3-way, not 2-way
like MIPS originally was** -- ported from goken's `il/noop.c`
directly: (1) leaf, no locals: no prologue/epilogue at all, RET is
just `JMP (RLINK)`; (2) leaf, with locals (declares a nonzero frame
but makes no calls, so RLINK is never clobbered): prologue only
adjusts SP, no RLINK save, RET's epilogue just adjusts SP back and
jumps; (3) not leaf: full save-adjust-SP prologue and
restore-adjust-SP-jump epilogue. Built correctly from the start here
since the C source was already open -- and this same 3-way shape
turned out to be a real, previously-latent MIPS bug too (see
`mips_port.md`'s own "Post-completion fix" section, found while
writing this doc's own leaf/frame story and cross-checking `vl/
noop.c`).

**A real, confirmed bug: `Rewritei.ml`'s own RLINK save/restore was
using the wrong "which mnemonic width" tag.** The `move_size` tag
`W__` means "MOVW" -- always 32-bit, regardless of arch (confirmed
against goken's own `optab.c`: `AMOVW`'s row is unconditional, never
`is_64`-gated). But RLINK itself is a full pointer, 8 bytes wide on
riscv64 -- that's what a bare "MOV" (no width suffix) means in goken,
and bare "MOV" genuinely *is* `is_64`-dependent (confirmed
empirically: goken emits `SW` for bare `MOV` on riscv32 and `SD` on
riscv64). Conflating the two in one shared encoder arm (tagging
`Rewritei.ml`'s own save/restore as `W__` and `is_64`-branching
inside the encoder) was harmless until a genuine large-offset,
directly-user-written "MOVW" fixture on riscv64 needed the *other*
(always-32-bit) behavior from the exact same tag -- caught while
porting case 15/16. Fixed by giving `W__` and `V__` (the real
"vlong"/pointer-width tag) their own separate `Codegeni.ml` arms and
switching `Rewritei.ml` to build `V__` nodes for RLINK on 64-bit.

**A second real, confirmed bug of the same shape, found earlier
(case 5):** `Rewritei.ml`'s leaf-detection only cleared the leaf flag
for the label-targeted `JAL`/`JALR` forms, never for the new,
register-plus-offset `JALRI` -- so a function calling through
`JALR D,I(S)` was misclassified as a leaf and missed its RLINK-save
prologue entirely. Fixed by clearing leaf status for `JALRI` whenever
its destination register isn't the zero register (the JMP-spelled
form's own fixed default in this port's grammar, never
user-overridable -- an exact proxy for goken's own AJAL-vs-AJMP
identity check).

**Byte/halfword/32-bit-view memory access generalizes cleanly across
all three offset "distances"** (small/SB-relative-large/
arbitrary-register-large): `gen_store`/`gen_load` were refactored
from a thin "does it fit, else error" wrapper into helpers that
internally pick the small-offset (case 6/7) shape or synthesize the
large-offset (case 15/16, or case 12/13 for the SB-specific slow
path) LUI-based fallback themselves -- shared by every Move1/Move2
call site at once, so support for the large-offset cases "fell out"
for byte/half/word/doubleword together rather than needing separate
work per size.

**Storing/loading a global's *value* by symbol had zero codegen at
all**, not just a "large offset" gap -- `Ast_asmi.ml`'s `Entity` gen
constructor existed (used by `visit_globals_instr`) but nothing built
it (`gen`'s own grammar had no `name -> Entity` rule at all) and
nothing consumed it in `Codegeni.ml`. This is what case 12/13 turned
out to really be about, once investigated.

**Combining two GLOBLs of very different sizes shifts a small
global's own resolved offset by 4 bytes** between goken and xix -- a
data-segment layout/alignment discrepancy, confirmed unrelated to any
specific case's own encoding correctness (each global alone, in its
own fixture, is fully byte-identical). Same category of issue as
`mips_port.md`'s own "GLOBL-without-DATA total file size mismatch"
open issue -- not root-caused, worked around by keeping fixtures to
one global each.

**Case 17 (`fcvt`)'s funct7 term is easy to miss reading `asm.c`
alone** -- it's buried in the `OP_RF` macro's own definition, a
screen away from the case 17 body that calls it, not inlined at the
call site the way most other cases' bit-fiddling is. Every
(funct7, rs2-field, rounding-mode) triple for the 6 real conversion
directions (`MOVFD`/`MOVDF`/`MOVFW`/`MOVDW`/`MOVWF`/`MOVWD`) was
verified empirically against real goken output, not just derived
from the C source.

**Case 4's JAL/JMP/JALR-to-label had no range check at all** before
this was noticed while investigating case 18 -- an out-of-range
target (beyond the direct J-type immediate's ±2²⁰-byte reach) would
have silently produced wrong bytes. `fits_jal_range` now errors
loudly instead. The check has to live inside the lazily-evaluated
`binary` thunk, not evaluated eagerly at match-arm level -- eagerly
checking `branch_delta` broke an existing, previously-passing fixture
outright, since `real_pc` isn't finalized yet during the sizing pass
that runs before layout.

**goken's assembler applies real code-layout optimizations this port
doesn't replicate**, discovered via branch/jump fixtures: (1) a
"branch to L1, immediately followed by unconditional JMP to L2"
idiom (classic if/then/else) gets inverted and L1's code relocated to
the end of the function; (2) dead code after an unconditional JMP
gets elided; (3) combined with a later JAL/indirect-return in the
same function, a "loop rotation" was observed (via an ad-hoc test) to
miscompute a target into non-4-byte-aligned garbage. None of these
are replicated (same category as MIPS's own `sched.c` delay-slot
story) -- worked around by writing fixtures in shapes that don't
trigger them (e.g. "invert-and-skip-one-instruction" instead of
"branch + trailing JMP").

## Investigated and skipped (confirmed dead in goken itself)

- **Case 19** (`addiw $0,rs,rd`) -- goken's optab has a real
  `AMOVW C_REG C_REG` row mapping here (meant for re-sign-extending a
  32-bit value already in a register on riscv64), but `obj.c`
  *rewrites* `AMOVW`/`AMOVWU` to plain `AMOV` whenever neither operand
  is a memory reference, on both riscv32 and riscv64 -- confirmed
  empirically ("MOVW R5,R6" produces a plain ADD-with-REGZERO
  encoding on both `ia`/`il` and `ja`/`jl`, never `ADDIW`). Unreachable
  via any concrete goken syntax.
- **Case 21** (`lui I,D; s[lr]ai N,D`, a sparse-64-bit-constant
  shortcut via `vconshift`) -- the only place goken ever classifies a
  constant this way is permanently short-circuited off in `pass.c`
  via a literal `if(0 && ...)`, the classic "disabled, never
  re-enabled" idiom. Confirmed empirically too: a genuinely sparse
  64-bit constant on riscv64 produces an SB-relative DATA load
  instead, never this shape.
- **A bare, label-only "JAL label"** (no explicit register) --
  goken's own grammar always requires an explicit register for
  "JAL"/"JALR", so there's no real syntax to differentially test
  against; xix's own grammar still accepts it (shares the tested
  JALR-with-Absolute encoding path) as a building block with no
  dedicated fixture of its own.
- **A standalone "LUI $I,D"** -- goken's grammar parses it, but its
  own linker `optab.c` has no entry for `ALUI` at all ("illegal
  combination"). Only reachable in practice via the MOVW-immediate
  fast path (case 8), which *is* tested.
- **Register-to-register `MOVB`/`MOVH`/`MOVBU`/`MOVHU`** (case 10) --
  goken's own grammar has no register-to-register syntax for these
  mnemonics at all, even though the underlying machinery fully
  supports a register "from" operand. xix's own syntax is kept as a
  building block, verified only indirectly (it reuses the same
  already-tested SLLI/SRAI/ANDI encoding helpers as case 1/2).

## Deliberately out of scope (not investigated as dead, just not attempted)

- **`MULH`/`MULHSU`/`MULHU`** and the immediate CSR variants
  (`CSRRWI`/`CSRRSI`/`CSRRCI`) -- `Ast_asmi.ml`'s own opcode types
  have no constructors for them yet. (`MUL`/`DIV`/`DIVU`/`REM`/`REMU`
  themselves, and their riscv64-only `*W` siblings, ARE wired now --
  see the hello_libc section below.) `ADDW`'s own immediate form
  (`ADDIW`) only wires the fits-in-12-bits fast path -- real goken
  has no large-constant fallback for it at all (see the hello_libc
  RISC-V64 section's own bug writeup).
- **Unsigned float→int conversions** (`FCVT.WU.*`) -- goken itself has
  no `MOVFWU`/`MOVDWU` mnemonics for the reverse direction, though the
  int→float direction (`MOVUF`/`MOVUD`, real goken mnemonics after
  all) IS wired -- see the hello_libc section below.
- **`ArithF`'s own `ABS_`/`NEG_`** (unary, don't fit the shared
  2-or-3-register shape) and **`DIV_`'s single-precision form**
  (`DIVF`) -- no real closure needs them yet, `ADD_`/`SUB_`/`MUL_`/
  `DIV_` (double) and `CmpF`'s `EQ_`/`LT_`/`LE_` are wired.
- **Case 18's full LUI+JALR far-branch fallback** -- see "Open
  issues" below; guarded against silently-wrong bytes but not
  implemented.

## hello_libc integration test

Status: **complete**. Same idea as `arm_port.md`'s/`arm64_port.md`'s/
`mips_port.md`'s/`amd64_port.md`'s own equivalent sections (read
`arm_port.md`'s first -- this is the RISC-V32 sibling, same
methodology, run in a later session): beyond the hand-written
`riscv_diff/` fixtures above (each one object file, one `TEXT`, no
real linking), `tests/linker/hello_libc_riscv/` stress-tests the whole
pipeline against goken's own real `hello.c` (which calls into a real,
reusable `lib_core/libc/libc.a`), compiled via real `ic -S` for its
full transitive libc dependency closure (36 files, found by
`scripts/find-c-closure.py`'s BFS), assembled with `oia`, linked with
`oil`, and run under `qemu-riscv32`. The fixture is self-contained
(`hello.c`, `closure.tgz`, `test.sh`, `Makefile`, mirroring
`hello_libc_amd64/`'s exact structure) and needs no goken checkout to
run day-to-day. Unusually for this whole multi-arch effort, roughly
half of the 36 closure files (`fmt_dofmt.c.s`, `fmt_fltfmt.c.s`,
`fmt_nan64.c.s`, `fmt_strtod.c.s`, `port_vlrt.c.s`, and others) aren't
assembleable by goken's own real `ia` either, for reasons predating
and unrelated to this port -- `build-c-program.py` reports this
informationally and proceeds anyway, since the requirement is a
correctly *running* xix-built binary, not byte parity against a
reference that can't itself be built for these files.

**A 4th and 5th instance of this whole multi-arch effort's running
"goken's own `-S` output isn't valid re-assembleable input to itself"
bug family** (after the comma-padding artifact shared by 5c/7c/vc/6c,
MIPS's unsigned-wraparound branch-offset printing, and ARM64's R31
register-name bug):
1. A compiled `JAL`'s own `p->reg` is legitimately `NREG` at the
   object-file level (the *linker*'s own `AJAL` case substitutes
   `REGLINK` for it at encode time) -- but real `ia`'s grammar has no
   bare, register-less `"JAL target"` form at all (every real
   hand-written `.s` in goken's own tree always spells out `"JAL
   R1,target"` explicitly). Fixed in goken's own
   `compilers/ic/list.c`'s `Pconv`: print the same `REGLINK`
   substitution the linker itself already performs.
2. `"MOVW $0,off(R)"` (an immediate-zero store to memory) isn't a real
   single RISC-V instruction at all -- real goken's own `optab.c` only
   has `"AMOVW, C_ZREG, C_SOREG -> OSTORE"`, i.e. a *register* known to
   be zero (`REGZERO`), not a literal `$0` constant; real `ia` rejects
   the bare-immediate spelling outright. Worked around at this port's
   own `Codegeni.ml` level instead of goken's C source (unlike gap 1,
   since there's no goken reference to match against for the specific
   files that need this either way): treat a zero-valued store
   immediate as `RZERO`, the objectively correct real encoding.

**A real, confirmed bug in "MOV" vs "MOVW"'s own register-to-register
encoding, caught the hard way (twice).** An earlier version of this
session assumed the two mnemonics put their source register in
different operand slots of the shared `"ADD rd,x0,rs"` idiom, based on
a fixture (`"MOVW R0,R8"`) whose source happened to be `x0` itself --
indistinguishable from either operand order by construction. That
false "asymmetry" made it into this port's own code and comments;
re-verified later with a non-zero source (`"MOVW R8,R10"`) and found
real goken's own bytes actually match "MOV"'s own convention exactly
(`rs1=x0, rs2=source`, not the other way around) -- the two mnemonics
share one encoder after all. A cautionary tale for this whole session
series' own "verify against real bytes" discipline: a fixture that
happens to use the zero register as its one distinguishing operand
can silently validate the wrong theory.

**Roughly 20 more real gaps closed getting the 36-file closure to
assemble+link+run** (most invisible to any `riscv_diff/*.s` fixture
alone, all found by feeding the real closure through `oia`/`oil` and
reading the resulting error, or -- for the last two below -- by
actually *running* the linked binary): several previously entirely
unwired real mnemonics (bare `"MOV"`, `"MOVWU"`, `"MOVUF"`/`"MOVUD"`
-- unsigned int→float/double, real goken mnemonics confirmed against
`lex.c`/`optab.c` despite an earlier, now-corrected comment claiming
otherwise); the `BLE`/`BGT`/`BLEU`/`BGTU` pseudo-branch family (ALSO
real goken mnemonics, an earlier comment's claim otherwise was simply
wrong -- rewritten by the linker into the reversed-relation LT/GE
hardware form with both the condition *and* the two register roles
reversed, not just a condition remap: a first attempt that reused the
already-computed registers unchanged produced the right size but wrong
content, caught by hand-decoding real bytes); float move/arithmetic/
compare (`FMOV`/`ArithF`/`CmpF`, including their own real-but-
asymmetric `rs2/rs1/rd` mapping -- again only caught by hand-decoding
real bytes, an "obvious" left-to-right guess produced byte-identical
*size* but wrong *content*); float memory access (`FLD`/`FSD`, both
plain register-indirect and SB-relative, needed by nearly every
function touching a `double`); a completely unimplemented Local/Param
pseudo-frame addressing story (`"off(SP)"`/`"off(FP)"`, goken's own
`D_AUTO`/`D_PARAM`, including its own address-of form,
`"$sym+N(SP)"`) -- this port's own `Param`/`Local` AST names are
swapped relative to goken's `D_PARAM`/`D_AUTO` naming, see
`Codegeni.ml`'s own `resolve_entity` comment; `"$const(Reg)"`
(address-of an arbitrary-register indirect, a plain `ADDI`, NOT a
memory load -- mirrors `Parser_asmv.mly`'s own identical MIPS
production, confirmed the same real construct from the same real
source file on both arches); and the real RISC-V M-extension (`MUL`/
`DIV`/`DIVU`/`REM`/`REMU`, previously unimplemented despite existing
AST/grammar scaffolding, genuinely worth its own byte-verification
since division isn't commutative -- a left/right swap here would
silently compute the wrong *value*).

**Two real, confirmed bugs found only by *running* the linked
binary**, not by getting it to assemble+link (same category as
amd64's own "pseudo-SP" bug -- every individual instruction still
encoded to *some* valid byte sequence):

1. **A leaf function *with* a nonzero declared frame crashed the
   linker outright** (`"rewrite should have transformed virtual
   instrs"`) -- `Rewritei.ml`'s own RET-rewrite case 2 updated
   `n.next` but never `n.instr` itself, leaving the original
   un-transformed virtual `RET` node behind for codegen to choke on
   (unlike case 1 and case 3 just above/below it, which both correctly
   overwrite `n.instr`). A real, pre-existing bug, unrelated to and
   predating this session's own Local/Param work -- just first exposed
   by it, since a leaf function with real local variables is exactly
   when this shape comes up.
2. **`"MOV $sym+N(SB),R"` (address-of-a-global-with-a-nonzero-offset)
   silently discarded N entirely for every N≠0**, always computing the
   address of `"sym+0"` instead -- the offset field's own name in the
   `Codegeni.ml` match arm was literally `_offsetTODO`, an explicit,
   pre-existing "not implemented" marker never wired through any of
   the case's 3 address formulas. No earlier fixture in this whole
   port happened to use a nonzero address-of-global offset, so nothing
   caught it until a real program produced plausible-looking but
   *wrong* output: `fmt/dofmt.c`'s own `"%d"` digit-table setup,
   `"MOV $.string<>+12(SB),R13"` (picking a `"0123456789..."`
   sub-table out of a larger shared string-literal blob), landed on
   the WRONG sub-string, printing `"hello from libc.a: i + i = >"`
   instead of `"2 + 2 = 4"` -- confirmed once root-caused by hand-
   decoding the actual bytes at the wrong vs. right offset, and by
   reproducing the exact shape minimally (a hand-written multi-`DATA`
   `.string<>`-style blob addressed at a nonzero offset).
   Byte-identical differential testing against goken can't catch this
   class of bug in general (goken can't even assemble several of the
   files that exposed it in the first place) -- only a real end-to-end
   run, checking actual printed output rather than just exit code or
   crash-vs-no-crash, does.

A third, unrelated pre-existing bug was also found and fixed while
building the Local/Param-relative addressing fixtures above: a
one-pass linker symbol-table construction issue where a TEXT symbol
referenced *before* its own definition (a real forward reference --
`fmt/fmtfd.c`'s own `fmtfdinit` takes the address of
`fmt/fmtfdflush.c`'s `__fmtFdFlush`, defined in a *later* unit of the
same link) crashed with a raw `Not_found` instead of resolving once
layout completes -- fixed by deferring the lookup into the binary-
emission thunk, the same pattern the SData2 slow path's own
`init_data` lookup already used.

**Fixture discipline**: eight new byte-identical `riscv_diff/`
fixtures, one per gap area above with a real, verifiable encoding or
behavior difference: `float_arith_case17.s` (FMOV/ArithF/CmpF and
their real rs2/rs1/rd mapping), `float_mem_case17b.s` (FLD/FSD +
MOVUF/MOVUD), `sp_fp_pseudo.s` (Local/Param addressing, also exercises
the leaf-with-frame RET bug), `branch_pseudo_ble_bgt.s` (BLE/BGT/
BLEU/BGTU and their real operand-swap), `muldiv_ext.s` (the
M-extension, both 2- and 3-register forms), `addr_global_offset.s`
(the `_offsetTODO` bug -- the most important one, since it produced
wrong output rather than a crash or an assemble/link failure),
`fwd_text_ref.s` (the forward-TEXT-reference symbol-table bug), and
`addr_reg_indirect.s` (`"$const(Reg)"`).

## hello_libc RISC-V64 integration test

Status: **complete**. Same idea as the RISC-V32 section just above
(read that one first -- this is the RISC-V64 sibling, same
methodology, run in a later session): `tests/linker/hello_libc_riscv64/`
stress-tests the whole pipeline against goken's own real `hello.c`,
compiled via real `jc -S` for its full transitive libc dependency
closure (35 files -- one fewer than RISC-V32's own 36, since
riscv64's native 64-bit integers don't need `port/vlrt.c`'s software
int64-emulation helpers), assembled with `oja`, linked with `ojl`,
and run under `qemu-riscv64`. The fixture is self-contained (`hello.c`,
`closure.tgz`, `test.sh`, `Makefile`) and needs no goken checkout to
run day-to-day.

**Much smaller gap than RISC-V32's own effort, since `Codegeni.ml`/
`Rewritei.ml`/`Layouti.ml`/`Ast_asmi.ml` are all fully shared between
the two arches** -- every gap the RISC-V32 session closed (Local/
Param addressing, float arithmetic/compare, the M-extension, FLD/FSD,
the address-of-global offset bug, the leaf-with-frame RET bug, the
forward-TEXT-reference bug, and more) was already fixed before this
session started. The riscv64 closure linked and ran correctly
(printing the exact expected output) on the very *first* successful
link attempt, once the genuinely riscv64-specific gaps below were
closed:

- **The explicit-32-bit-view `*W` opcode family**
  (`ADDW`/`SUBW`/`SLLW`/`SRLW`/`SRAW`/`MULW`/`DIVW`/`DIVUW`/`REMW`/
  `REMUW`, plus `ADDIW`'s own immediate form) -- real RISC-V's own
  `OOP_32`/`OOP_IMM_32` major opcodes (`0x3b`/`0x1b`) instead of
  `OOP`/`OOP_IMM`'s `0x33`/`0x13`, otherwise identical funct3/funct7
  to their native-width siblings (confirmed against real goken's own
  `optab.c`). `SUBW`'s own immediate form gets the exact same
  negate-and-rewrite-to-`ADDW` treatment plain `SUB` already does
  (goken's own linker handles `ASUB`/`ASUBW` in the identical switch
  case). Previously entirely unimplemented -- `oprrr_arith_opcode`
  had a standing `failwith "TODO:...RV64 *W ops"` for every one of
  them, and `Ast_asmi.ml`'s own `MUL` constructor didn't even carry a
  `w option` yet (unlike `DIV`/`REM`, which already did).
- **A `V__` (bare, pointer-width) sibling for `"MOV R,sym(SB)"`/
  `"MOV sym(SB),R"` (store/load-to-global) and `"MOV $0,off(R)"`
  (zero-immediate store)** -- both already had a `W__` arm (always
  32-bit), but riscv64's own real 64-bit pointers need the genuinely
  is_64-dependent SD/LD-vs-SW/LW split every other `V__` arm in this
  file already has. Found stress-testing real lib_core/libc
  (`port/mainargs.c`'s own real `"MOV R9,_mainargv(SB)"`, storing a
  real pointer; `fmt/dofmt.c`'s own real `"MOV $0,16(R2)"`).
- **A 6th instance of this whole multi-arch effort's running
  "goken's own `-S` output isn't valid re-assembleable input to
  itself" bug family.** A compiled `Prog`'s own small, valid `ADDIW`
  immediate (e.g. `-1`) gets printed by `ic`'s own `Pconv` as its
  *unsigned* 32-bit representation (`"$4294967295"`) instead of the
  signed one (`"$-1"`) -- real goken's own `ja`/`jl` then reject that
  exact spelling outright (`"illegal combination"`, confirmed
  empirically: `ADDIW`'s own operand-class check has no large-
  constant fallback at all, unlike plain `ADDI`'s own case
  9/14-style `LUI`+`ADDI` expansion). Since there's no goken reference
  to byte-match against for this literal spelling either way, worked
  around at this port's own `Codegeni.ml` level: reinterpret the raw
  immediate as a 32-bit signed quantity (sign-extend from bit 31)
  *before* the fits-in-12-bits check, so `"$4294967295"` and `"$-1"`
  produce identical bytes -- verified by comparing this port's own
  output for both spellings against each other (no goken reference
  possible), and confirmed correct via a `qemu-riscv64` run checking
  the actual computed value. Found stress-testing real lib_core/libc
  (`port/strtod.c`'s own real `"ADDW $4294967295,R11,R12"`).

**Fixture discipline**: two new byte-identical `riscv64_diff/`
fixtures (`w32_variants.s`, `v_pointer_global.s`) plus one functional-
only fixture not wired into `test-riscv64.sh`'s own `CASES` array
(`addiw_signext.s` -- no goken reference exists for it either way,
same reasoning as the two zero-immediate-store gaps above; verified
by comparing this port's own two immediate spellings against each
other, plus a `qemu-riscv64` run).

## Open issues

- **Case 18 (far JAL/JMP/JALR-to-label)** is a genuine
  assembler-level "does this reach" decision (closer in scope to
  ARM32/MIPS's own multi-pass branch-range stories than an isolated
  encoder function), deferred rather than rushed -- no current
  fixture's functions are anywhere near the ±2²⁰-byte range limit.
  Guarded (`fits_jal_range`) so an out-of-range target errors loudly
  instead of silently corrupting output; revisit if a real program
  ever needs functions that far apart.
- **RV64's address-of-procedure uses AUIPC in goken, LUI+ADDI here**
  -- functionally correct (confirmed by matching exit codes across
  fixtures that exercise it indirectly) but not byte-identical.
  Surfaced while testing case 5; worked around by reaching test
  callees via the already-verified JAL mechanism instead of an
  address-of-procedure load. Revisit `Codegeni.ml`'s case 9/20
  AUIPC-vs-LUI dispatch for RV64 specifically if a fixture ever needs
  `"$proc(SB)"` directly.
- **A data-segment layout/alignment discrepancy** when combining two
  GLOBLs of very different sizes in one program (a small global's own
  resolved offset shifts by 4 bytes between goken and xix). Not
  root-caused; same category as `mips_port.md`'s own open
  GLOBL-layout issue. Worked around in every affected fixture by
  keeping one GLOBL per program.
- **goken's real large-64-bit-constant story isn't implemented at
  all** -- confirmed (see case 21's own writeup) that goken falls
  back to an SB-relative DATA load for a constant too big for case
  9/14's own LUI+ADDI range, a mechanism this port hasn't built.
  Likely rare in practice; not investigated further.
