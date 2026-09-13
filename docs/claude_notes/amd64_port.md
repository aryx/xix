# Porting the amd64 toolchain (6a/6l) against goken, byte-equal

Status: **complete** for the scoped feature set. `o6a`/`o6l` exist,
and twenty-one fixtures assemble+link to executables byte-identical
to goken's real `6a`/`6l` output, verified under `qemu-x86_64`.
Covered: the full Q/L/W/B-width integer ISA (ADD/SUB/XOR/AND/OR/CMP/
TEST/MOV, including the `0x81` imm32 form and goken's own `Yi32`
immediate class), SHL(=SAL)/SHR/SAR shifts, sign/zero-extending moves
(MOVBLSX/MOVBLZX/etc, MOVLQSX/MOVLQZX), NEG/NOT/INC/DEC,
MUL/IMUL/DIV/IDIV (both the single-operand and IMUL's own 2-operand
form) plus CWD/CDQ/CQO, CMPXCHG+LOCK, LEAQ (address-of-global),
direct+indirect CALL/JMP, short-form (rel8) Jcc, RET, SYSCALL, `foo<>`
static symbols, true 64-bit MOVQ immediates, and single- and
double-precision SSE floating point (MOVSD/MOVSS, ADDSD/SUBSD/MULSD/
DIVSD + SS siblings, UCOMISD/UCOMISS, both int widths of
CVTS{L,Q}2S{D,S}/CVTTS{D,S}2S{L,Q}, CVTSD2SS/CVTSS2SD, XORPD/XORPS's
self-clear idiom, a raw GP&harr;XMM MOVQ bit-copy, and PSLLQ). A full
amd64-wide mnemonic sweep -- every buildable file under `tests/c/*.c`
and every real, hand-written amd64 `.s` file anywhere in goken's own
tree -- confirms nothing else is missing. Deliberately out of scope:
see "Deliberately out of scope" and "Open issues" below.

## Goal

Same methodology as `arm_port.md` (ARM32), `mips_port.md` (MIPS),
`arm64_port.md` (ARM64), and `riscv_port.md` (RISC-V): assemble+link
the same `.s` with both goken's 6a/6l (Plan 9 amd64 assembler/linker,
`~/goken`) and xix's own o6a/o6l, and require the final executables to
be byte-identical. Scope is amd64 assembler + linker only, permanently
-- the compiler (6c) is out of scope for the exact same reason as
every other arch (occ's plan is c--'s backend, not per-arch goken
compiler parity).

Read `arm_port.md` first for the harness shape, syncweb rules,
where-changes-land guidance, and goken-flag conventions shared across
every arch in this effort -- this doc only calls out what's
amd64-specific.

## The one load-bearing difference from every arch ported so far

amd64 is the first genuinely CISC, variable-length-instruction arch in
this whole differential-testing project (ARM32/MIPS/ARM64/RISC-V are
all fixed-4-byte-instruction RISC machines). Three consequences, each
a real, non-cosmetic design point:

1. **The shared linker's executable writer assumed 4-byte words.**
   `linker/Execgen.ml`'s text-segment parameter was `Types.word list`,
   and `Codegen{5,7,i,v}.ml`'s own `gen` functions hard-asserted
   `List.length instrs * 4 = size`. Generalized (with the user's
   explicit sign-off first, since it touches all 4 already-completed
   ports): `Execgen.gen` now takes `Types.byte array` for text
   uniformly (it already did for data), with a new
   `Types.bytes_of_words` helper flattening each existing arch's own
   `word list` into bytes, *endian-aware* (`Arch.endian_of_arch`) --
   MIPS is genuinely big-endian here, so an earlier draft that
   hardcoded `Little` would have silently corrupted every `ovl`
   output; caught before it ever landed. `Codegen6.ml` produces
   `Types.byte array` directly, no word/bitfield packing at all
   (`Bits.int32`'s own `sanity_check_32` hardcodes a 32-bit budget --
   doesn't fit a 1-15-byte instruction stream). Verified: full 4-arch
   regression suite still green after this change, before amd64 itself
   was even wired up.

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
   arch-specific link-register-save-slot size.

   A direct consequence: amd64 needs **no auto-generated
   prologue/epilogue** at all (`Rewrite6.ml` is a genuine no-op) --
   `CALL`/`RET` push/pop the return address in hardware, and any real
   stack adjustment is already explicit source-level instructions
   (goken's own `hello_linux_amd64.s` does its own "SUBQ $16,SP" /
   "ADDQ $16,SP"), unlike ARM32/MIPS/RISC-V/ARM64's own
   frame-size-driven synthesis. Similarly **no literal pool**
   (`Layout6.ml` is a plain PC-accumulation walk, no pool-splicing
   machinery at all) -- LEAQ's absolute address and any move-immediate
   are both encoded inline in the instruction stream, not through a
   separate pool the way ARM32/ARM64/MIPS/RISC-V all need one.

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
- Every encoding rule was additionally byte-verified against real
  `6a`/`6l` output (`qemu-x86_64 -d in_asm` trace and manual
  REX/ModRM/SIB decoding by hand, since `objdump` wasn't reliably
  available for cross-arch ELF on this host) *before* being ported,
  not derived from reading the C source alone and trusted blind.
- 6l's own codegen dispatches by named *shape* (`optab.c`'s Z-code
  column, switched on by name in `span.c`'s `doasm()`), not by a
  numbered `case NN` per mnemonic the way the RISC archs'
  `codegen.c`/`asmout.c` files do -- a real CISC-vs-RISC codegen
  strategy difference (many mnemonics share a handful of encoding
  shapes here, rather than one switch-case per AST-level operation).
  `Codegen6.ml`'s own match arms are annotated with `(* case Zxxx *)`
  tags naming the real `span.c` case each one mirrors, the direct
  equivalent of `Codegen5.ml`/`Codegen7.ml`'s own `case NN` comments.

**Instruction selection was driven by real usage, not guesswork.**
Once the core arithmetic/move/branch ISA was in place, remaining
batches were prioritized by two concrete methods rather than
abstractly guessing what's "essential": (1) `6c -S` (goken's own C
compiler; its own assembly output isn't reassemblable, same caveat as
[[hello-libc-integration-test]]'s `5c -S` finding, but it's perfectly
readable for mnemonic *names*) run across nearly every buildable file
under `tests/c/*.c` (57 of ~60 compiled cleanly), tallying mnemonic
frequency; (2) a full mnemonic grep across every real, hand-written
amd64 `.s` file anywhere in goken's own tree
(`lib_core/libc/arch/amd64/`, `lib_core/libc/os/windows/`,
`lib_core/libc/syscall/os/*/`, `tests/s/*`), diffed against what this
port already parsed. Together these found AND/OR, shifts, the imm32
arith form, sign/zero-extending moves, NEG/NOT/INC/DEC, MUL/IMUL/
DIV/IDIV, TEST, CMPXCHG+LOCK, a third SSE conversion story, and
XORPD's self-clear idiom -- real gaps a purely abstract "most
essential instructions" judgment call had missed. The second method
in particular surfaced instructions the first would never have found
(TEST and CMPXCHG appear only in hand-written files, never in any
sampled 6c output), and a genuinely more fundamental gap only
discoverable by actually *compiling* a real file rather than grepping
mnemonics: `encode_rm` only supported SP as a memory base, but
goken's own hand-written `cas()` primitive (`tests/c/float/
linux_amd64.s`) addresses through `BX`.

## Real findings, gotchas, and design decisions

**goken's real `6l` needs an explicit `-S` flag to emit ELF section
headers at all.** Confirmed in `~/goken/linkers/liblk/elf.c`
(`elf32`/`elf64` both gate every section-header write behind
`if(debug['S'])`). Without it, goken's own 6l silently writes
`e_shoff=0`/`shnum=0`, a real, `6l`-specific default that `5l`/`7l`/
`8l` apparently don't share (their own existing `diff-*.sh` scripts
never pass `-S` and still get section headers from goken) -- not
root-caused further, just worked around by adding `-S` to
`scripts/diff-amd64.sh`'s own goken invocation.

**A build-artifact-staleness gotcha, a variant of
[[arm64-linker-port]]'s own "dune build --force can still be
insufficient" note**: `_build/default/bin_dune/o6a`/`o6l` (the path
every existing `diff-*.sh` script uses) went stale after an edit +
`dune build --force` on at least one occasion, while
`_build/default/linker/Main.exe` and the top-level `bin_dune/` symlink
were both already fresh. Worked around by having
`scripts/diff-amd64.sh` use the `bin_dune/` symlink path instead --
confirmed that one always tracks the freshest `Main.exe`. Prefer
`bin_dune/oXX` over `_build/default/bin_dune/oXX` outright for any
future arch's own diff script too, rather than re-debugging this.

**Each width's own opcode family follows one consistent, exploitable
pattern: byte-width (B) opcodes are always exactly one less than the
shared Q/L/W opcode** (confirmed across `Arith`'s `Zr_m` family,
`Cmp`'s `Zm_r`, `Move`'s `Zr_m`/`Zm_r`, `Shift`'s `Zo_m` family,
`Test`, and `CmpXchg` -- e.g. ADD's reg-reg opcode is `0x00` at byte
width vs `0x01` at L/Q/W, matching real x86's own "even opcode = 8-bit
form" encoding convention) -- kept as explicit per-width tables
throughout rather than "opcode - 1" arithmetic, so a future opcode
this pattern doesn't hold for can't silently rely on it.

**B-width's real REX-forcing quirk, and a real bug in this port's own
first attempt at it.** Real amd64 ModRM/opcode-embedded register-field
values 4-7, at byte width with *no* REX byte present, name the legacy
high-byte registers AH/CH/DH/BH; a REX byte (even an otherwise-empty
`0x40`) switches those same field values over to meaning
SPL/BPL/SIL/DIL instead. This port's register model has no separate
AH/BH/CH/DH token, so whenever the grammar names SP/BP/SI/DI as a
byte-width *register value*, a REX byte must be forced -- confirmed:
"MOVB $6,SI" -> `40 b6 06`. The first implementation applied this
check to `reg_field` unconditionally, without checking whether
`reg_field` was actually a register -- broke "SUBB $1,BX"/"CMPB
BX,$5" (`reg_field` there is a fixed opcode-extension digit that
happens to number 4-7 for SUB/XOR/CMP, not a register: real 6a/6l is
`80 eb 01`/`80 fb 05`, no REX at all) and "MOVB AX,-8(SP)" (SP used as
a *memory addressing base*, not a register value: real 6a/6l is
`88 44 24 f8`, no REX). Fixed with an explicit `reg_is_register` flag
threaded through `rex_opt`, and narrowing the `rm`-side check to
`RReg` only (a plain register, never a memory base). Caught
immediately by a fixture's own byte comparison before being committed.

**SP itself has a real, narrower gap in goken's own reference
implementation: it can't be used as a byte-width register value at
all.** `span.c`'s `oclass()` has a `case D_BPB: case D_SIB: case
D_DIB: ...` block covering BP/SI/DI's byte forms, but the parallel
`case D_SPB:` line is commented out in this vanilla-imported goken
source. Confirmed empirically: "MOVB $5,SP" assembles fine under `6a`
(which only parses -- see the whole project's own 6a/6l split) but
fails at `6l`'s own `doasm()` with `notfound`, since the row search
can't classify `D_SPB` at all. BP/SI/DI/R8-R15 all work fine. Since
there's no goken reference output to match, this port doesn't support
it either.

**A real, non-obvious row-order asymmetry between integer MOV and
MOVSD.** `ymovl`'s own table checks the *store* row (`Zr_m`, opcode
`0x89`) before the *load* row (`Zm_r`, `0x8b`), so this port's `Move`
matches its store clause first for a plain register-to-register
move -- but `yxmov` (MOVSD/MOVSS) lists them in the *opposite* order
(load first), confirmed: "MOVSD X0,X1" (a plain reg-reg move, where
both rows would otherwise apply) -> `f2 0f 10 c8`, the *load* opcode,
not `0x11`. Caught by checking real byte output before writing
`MovF`'s own codegen clauses, not as a bug fix afterward -- assuming
"MOV-shaped instructions all order their y-table rows the same way"
would have produced a fixture that silently failed only on the
reg-reg case, not the reg-mem ones (which agree either way).

**A real bug in this port's own first attempt at MOVL's immediate
form**: assumed, by loose analogy with CMP's own `ycmpl` (which
genuinely has no `Yi0` row), that only `ymovq` -- not `ymovl` -- would
need a `MOVL $0,R` self-XOR (`Zclr`) special case. Both `ymovq` and
`ymovl` (and `ymovw`) have this row; `ymovb` genuinely doesn't
(confirmed: "MOVB $0,AL" is an ordinary `Zib_rp` immediate move,
`b0 00`, not a self-XOR). Fixed uniformly across Q/L/W widths, one
`Move (width, Right (Int 0), GReg r)` case.

**`Move`'s own immediate range needs to match goken's full `Yi32`
class, not just the narrower `Ys32` subset** -- a real gap found while
testing `Extend`, with a genuinely surprising semantic consequence.
goken's own `oclass()` (`span.c`) classifies a *positive* hex literal
like `$0xFFFFFFF6` (parsed as the literal value 4294967286, not -10)
as `Yi32` -- distinct from `Ys32` (sign-extendable 32-bit) purely by
*how the value was written*, not its final bit pattern: `l = v;
if((vlong)l == v) return Ys32; if((v>>32)==0) return Yi32;` (`l` a
32-bit local). For a *register* destination this reaches `Ziq_rp`'s
own case body, which has an internal `l = v>>32; if(l==0){ clear
REX.W; emit 0xb8+reg; put4(v); }` downgrade this port's earlier,
narrower guard didn't implement (confirmed: "MOVQ $0xFFFFFFF6,AX" ->
`b8 f6 ff ff ff`, no REX at all, not the REX.W+0xc7 form the
Ys32-only guard produced for the bit-pattern-identical `$-10`).
`ymovl`/`ymovw` have no `Ys32`/`Yi32` split at all, so those two
widths needed only the range guard widened, no encoding change. The
surprising part: the *same* Yi32 immediate produces different final
64-bit *values* depending on the destination -- a register
(`Ziq_rp`'s downgrade) zero-extends, memory (`Zilo_m`) sign-extends,
matching real amd64's own ordinary-32-bit-write-zero-extends vs
MOV-r/m64-imm32-sign-extends semantics exactly (confirmed and pinned
down in `imm_yi32.s`).

**`MOVLQZX` looks like a trivial alias for plain `MOVL` but isn't
byte-identical for the reg-reg case.** Real amd64 has no dedicated
"zero-extend 32→64" opcode at all (an ordinary 32-bit register write
already implicitly zero-extends), so goken's own `AMOVLQZX` table
entry really is just the ordinary `0x8b` load opcode. But that table
has *only* the load-direction row, while `Move`'s own reg-reg case
picks the *store* direction first (see the row-order finding above)
-- aliasing `MOVLQZX` directly onto `Move`'s existing codegen produced
the wrong bytes for the reg-reg case specifically (`89 d8` instead of
goken's own `8b d8`), caught by testing the actual reg-reg shape
rather than trusting the "it's just MOVL" reasoning alone. Routed
through `Extend`'s own dedicated, always-load-direction codegen
instead.

**`encode_rm` only supported SP as a memory base until a real
hand-written fixture (`cas()`) needed BX.** Widened to accept any
*ordinary* register as a plain memory base (no SIB needed) -- BP/R13
(mod=00/rm=101 means RIP-relative in real amd64, not "no
displacement") and R12 (the same mandatory-SIB quirk SP itself needs)
still aren't wired and still raise `Todo`.

**A real grammar mistake, caught immediately by the next full-suite
run, never shipped.** Adding the raw GP&harr;XMM MOVQ shape as two
*additional* grammar alternatives (`TMOV gen TC xreg` / `TMOV xreg TC
gen`) alongside the existing `TMOV lgen TC gen` broke *every* ordinary
"MOVQ Rs,Rd"/"MOVQ $imm,Rd" fixture -- a genuine LALR(1) shift/reduce
conflict (ocamlyacc warned, then silently picked one interpretation)
from three productions sharing a "TMOV gen TC ..." prefix that only
diverges *after* the comma, which 1-token lookahead can't resolve.
Fixed by left-factoring into one shared production (`TMOV
move_operand TC move_operand`, a local 3-way union type disambiguated
in the semantic action instead of by competing grammar rules). General
lesson for any future arch: when a new operand shape needs to share a
leading token with an existing production, add it as a new
alternative *inside* the existing rule's own operand nonterminal (or
left-factor explicitly like this), never as a sibling top-level
production with the same prefix.

**Three different SSE prefix stories, each confirmed rather than
assumed by analogy.** UCOMISD's own prefix is `Pe` (`0x66`), not the
`Pf2` every other double-precision instruction here uses (confirmed:
"UCOMISD X1,X0" -> `66 0f 2e c1`). UCOMISS has *no* legacy prefix byte
at all (`optab.c`'s own `Pm` -- just the `0x0f` escape itself;
confirmed: "UCOMISS X1,X0" -> `0f 2e c1`) -- checked explicitly rather
than assuming "swap `Pf2` for `Pf3` everywhere" would cover
UCOMISD→UCOMISS the same way it covers every other SSE instruction
here, since UCOMISD's own prefix wasn't the `Pf2` pattern to begin
with. And `CVTSD2SS`/`CVTSS2SD` share one real opcode (`0x5a`) for
*both* directions, disambiguated purely by which legacy prefix is
used -- the *source*'s precision picks it, the one place in this file
`A.floatp_precision` names the source rather than the operation's own
nominal precision.

**Real amd64 (and real 6a/6l) has no float-immediate encoding at
all.** "MOVSD $0,X0" is accepted by `6a`'s own parser but rejected by
`6l`'s `doasm()` with `notfound` -- the same "6a parses, 6l encodes"
split already seen for the SP-byte-register gap above. Matches every
other arch's own choice to skip float immediates entirely (goken's
own reference implementation treats its float-immediate support as
dead code too). Every float value in this port's own fixtures is
built via `CvtIntToF` from an integer instead.

**Indirect `CALL`/`JMP` and `LEAQ` both had real, non-obvious corners
found while wiring them up.** Real 6a's indirect CALL/JMP takes a bare
register with no parens ("CALL BX", not "CALL (BX)") -- a real
difference from the parenthesized `(R1)` convention this arch's own
`ireg` grammar rule was copied from ARM64's template with; both forms
turned out to be valid real 6a syntax, so this arch's grammar accepts
both. A forward reference to a *procedure* ("LEAQ later_proc(SB),R"
naming a TEXT symbol declared further down the same file) crashed
with `Not_found` during the sizing pass, since (unlike a DATA global,
fully resolved upfront) a TEXT symbol's own table entry is only added
incrementally as the sizing walk reaches each `TEXT` pseudo-op --
fixed the same way as `Call`/`Jmp`/`Jcc`'s own real_pc-dependent
values (size computed eagerly with a placeholder address, the real
lookup deferred into the lazily-evaluated `binary` thunk). And a
same-function local label's address can't be taken with LEAQ at all
in real 6a ("LEAQ label(SB),R" for a label inside the same TEXT
errors) -- only real global symbols work as LEAQ's target.

**goken's real `6l` applies real code-layout optimizations around an
unconditional jump that this port doesn't replicate.** Found while
first trying to test `Jmp`/`JMP`: a "JMP L; &lt;dead code&gt;; L:"
fixture (dead code padded past the short form's 127-byte range to
force the near/rel32 form) assembled to *zero bytes* for the JMP and
everything it skipped. This extends to **loop rotation**: a `loop:
cmp;jeq exit; body; jmp loop` structure gets goken's own comparison
duplicated into a trailing conditional branch, eliminating the
unconditional backward jump entirely (a genuine "while &rarr;
do-while with a duplicated leading check" transformation). Both are
specific to the *unconditional* jump -- a conditional (`Jcc`)
forward skip's own fallthrough is never elided. Root cause not fully
chased (plausible given goken's own Go-toolchain lineage: real
compiler-style optimizations, not just assembly). Consequence: a
byte-identical fixture for direct `Jmp` needs to avoid both patterns,
which rules out the two most natural ways to exercise it in a small
test -- see "Open issues" below. Relatedly, goken's real relaxation
between a Jcc/JMP's short (2-byte, rel8) and near (5/6-byte, rel32)
forms is a genuine multi-pass sizing problem (`span.c`'s `Zbr`/`Zjmp`
cases pick based on the actual resolved distance) -- not attempted;
only the short form is wired.

**LOCK is a genuinely bare, standalone pseudo-instruction in goken's
own model, not a prefix baked into the next instruction's own
encoding.** `{ALOCK, ynone, Px, 0xf0}` -- goken emits the raw `0xf0`
byte as `LOCK`'s own complete encoding, on its own source line before
the instruction it modifies, relying on x86 hardware to read it as a
prefix for whatever follows. This port doesn't validate what follows
it either, matching goken's own laxness. `JZ` (a real, hand-
maintained alternate spelling for `JEQ` in goken's own lexer,
`"JZ", LTYPER, AJEQ, /* alternate */`) is wired as a plain alias, same
opcode either way.

## Deliberately out of scope (not investigated as dead, just not attempted)

- **`NAME = value` constants** -- a real, cross-arch limitation, not
  amd64-specific (`Parser_asm.ml`'s own header comment: deliberately
  unimplemented everywhere), see [[hello-libc-integration-test]] for
  the same gap found on ARM.
- **RIP-relative addressing** (needed the moment a fixture targets a
  non-Linux `HEADTYPE`, or if this project ever wants
  position-independent amd64 output) and **indexed addressing**
  (SIB.index, hence REX.X) -- REX.X is always 0 here.
- **BP/R12/R13 as a memory base** -- BP/R13 need a real ModRM special
  case for `[rip+disp32]` (mod=00/rm=101 means RIP-relative in real
  amd64, not "no displacement"); R12 needs the same mandatory-SIB
  quirk SP already gets. Every *other* register works as an ordinary
  memory base now (see "Real findings" above).
- **BP as a plain register token** -- not wired in this arch's own
  grammar at all, not just as a memory base; only
  SP/AX/CX/DX/BX/SI/DI/R8-R15 are.
- **Legacy AH/BH/CH/DH byte-register forms** -- this port's register
  model has no token for them; whenever SP/BP/SI/DI are named at byte
  width, they always mean the low-byte (SPL/BPL/SIL/DIL) forms, see
  the REX-forcing finding above.
- **Indirect CALL/JMP *through memory*** -- only through a register is
  wired (goken's own `Yml` class also accepts memory here).
- **IMUL's own 3-operand immediate forms** (goken's `Zib_rr`/
  `Zil_rr`) -- real but rarer than the 2-operand form already wired.
- **x87** -- single- and double-precision SSE both landed; x87 is
  deferred indefinitely absent a concrete need (real amd64 userspace
  code essentially never uses it; SSE is the real ABI convention).
- **The 32-bit-int forms of `Extend`'s own opcode family beyond what's
  wired**, and any byte-suffixed forms of the `Extend`/`MulDiv`/
  `Unary`/`Test`/`CmpXchg` families beyond what's already covered by
  the width-generic tables.

## Open issues

- **Direct (non-indirect) `Jmp`/`JMP` has no byte-identical fixture at
  all**, and Jcc/Jmp's near-form (rel32) relaxation isn't implemented
  -- both blocked on the same real obstacle (see "Real findings"
  above): goken's own dead-code elision and loop rotation around an
  unconditional jump make the two most natural ways to force/exercise
  the near form actively misleading to test against, and the near-form
  relaxation itself is a genuine multi-pass sizing problem (closer in
  scope to ARM32/MIPS/RISC-V's own branch-range stories than an
  isolated encoder function). Every fixture so far avoids the patterns
  that trigger either. Revisit with either a large enough real program
  that naturally needs the near form without artificial dead-code
  padding, or by root-causing goken's own transformation precisely
  enough to construct a fixture around it deliberately.
