# Porting the amd64 toolchain (6a/6l) against goken, byte-equal

**Status: fifteenth checkpoint reached.** `o6a`/`o6l` exist, and
seventeen fixtures assemble+link to executables **byte-identical** to
goken's real `6a`/`6l` output, with identical `qemu-x86_64` behavior:
`hello_linux.s` (goken's own real hello-world, exit 0), `cmp_jcc.s`
(CMPQ + JEQ/JNE/JLT/JGE, exit 42), `r8_r15.s` (R8-R15 across every
instruction, exit 12), `movl_arith.s` (32-bit MOVL/ADDL/CMPL,
including the register- vs memory-destination MOVL-immediate split and
the Zclr $0 optimization, exit 135), `movw_arith.s` (the same shape at
16-bit width, exit 135), `movb_arith.s` (the same shape at 8-bit width,
exercising the AL-implicit-opcode special case and the SI/DI
REX-forcing quirk -- see "Real bugs/quirks" below, exit 135),
`float_sd.s` (double-precision SSE: MOVSD/ADDSD/SUBSD/MULSD/DIVSD/
UCOMISD/CVTSQ2SD/CVTTSD2SQ, exit 12), `float_ss.s` (the same shapes at
single precision, exit 12), `andorshift.s` (AND/OR, SHL/SHR/SAR across
all three shift-amount shapes, and the AX-implicit-opcode imm32 form,
exit 99), `imm32_arith.s` (the general, non-AX imm32 arith/cmp form,
exit 88), `extend.s` (MOVBLSX/MOVBLZX/MOVWLSX/MOVWLZX/MOVLQSX/MOVLQZX,
exit 236), `imm_yi32.s` (Move's own widened `Yi32` immediate range --
see "Real bugs/quirks" below, exit 77), `unary.s` (NEG/NOT/INC/DEC
across Q/L/B width and R8-R15, exit 115), `muldiv.s` (single-operand
MUL/DIV/IDIV, IMUL's own 2-operand form, and CDQ/CQO, exit 220),
`indirect_call_jmp.s` (indirect CALL/JMP through a register, exit 7),
`static_symbol.s` (`foo<>` local symbols, exit 0), `imm64.s` (true
64-bit MOVQ immediates, exit 127).
`./test-amd64.sh` runs all seventeen.
Zero regressions across all 4 already-complete ports' own full suites
(`test-arm.sh` 54/54, `test-mips.sh`, `test-arm64.sh`, `test-riscv.sh`,
`test-riscv64.sh`) and `make test` (134/134) after every batch.

This checkpoint's batch was steered by grepping real 6c-compiled
output (`6c -S`) across several of goken's own `tests/c/*` files for
mnemonic frequency, rather than guessing what's "essential" in the
abstract -- see "Real bugs/quirks" below for the exact method and
counts.

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
- `Arith` (ADD/SUB/XOR, **Q, L, W, and B width**): register
  destination, immediate source (only when the immediate fits signed 8
  bits for Q/L/W -- goken's `Yi8`/opcode `0x83` -- or the full unsigned
  byte range for B, which has no wider form at all) or register source
  (goken's `Zr_m`; B has its own, genuinely different opcodes --
  `0x00`/`0x28`/`0x30` -- always exactly one less than Q/L/W's shared
  `0x01`/`0x29`/`0x31`, confirmed against `optab.c`, kept as an
  explicit table rather than "opcode - 1" arithmetic). W (16-bit)
  reuses the exact same opcodes as L (goken's `yaddl`/`yxorl` are
  shared across both), only adding the mandatory `0x66` "Pe"
  operand-size prefix -- see `Move`'s own note below for where that
  prefix sits relative to REX. B has a further special case: a
  destination of exactly AX (register index 0, i.e. AL) takes goken's
  own opcode-alone-no-ModRM "op AL,imm8" row (`0x04`/`0x2c`/`0x34`)
  *ahead of* the general ModRM form, since `yxorb`'s own table lists it
  first -- unlike Q/L/W, where the equivalent `Yax` special case sits
  *after* the general ModRM row and so is only reachable for an
  immediate too big for imm8 (not wired, out of scope) -- see "Real
  bugs/quirks" below.
- `Move` (MOVQ/MOVL/MOVW/MOVB): register<->register, register<->memory
  (goken's `Zr_m`/`Zm_r`, opcodes `0x89`/`0x8b` for Q/L/W, `0x88`/`0x8a`
  for B -- same "B is one less" pattern as `Arith`), `$0`-to-register
  (goken's `Zclr` self-XOR optimization -- **Q/L/W only**: a real bug
  in this port's own first attempt assumed only `ymovq` had this row;
  `ymovb` genuinely has *no* such row at all, confirmed against real
  6a -- "MOVB $0,AL" is an ordinary `Zib_rp` immediate move, `b0 00`,
  not a self-XOR), and immediate-to-register-or-memory when the
  immediate fits the width's own range (32 bits signed for Q/L, 16 for
  W, the full unsigned byte range for B) -- MOVQ always via
  `Zilo_m`/`0xc7 /0`, but MOVL/MOVW/MOVB's own tables put the simpler
  `Zil_rp`/`Zib_rp` (`0xb8+reg` for L/W, `0xb0+reg` for B -- no ModRM at
  all, same family as `Ziq_rp`, just a narrower immediate) *before*
  `Zilo_m`/`Zibo_m`, so a register destination takes that path instead
  and only a memory destination falls through to the ModRM form -- a
  real, non-obvious shape difference from MOVQ. W's own mandatory
  `0x66` prefix comes *before* any REX byte (confirmed against real
  6a: "MOVW AX,R9" -> `66 41 89 c1`), matching real x86's
  prefix-ordering rule (legacy prefixes precede REX, which must
  immediately precede the opcode); B needs no prefix byte at all (its
  own "Pb" is not a real prefix, see below).
- **B-width's real REX-forcing quirk** (`regrex_forces_rex` in
  `Codegen6.ml`): real amd64 ModRM/opcode-embedded register-field
  values 4-7, at byte width with *no* REX byte present, name the
  legacy high-byte registers AH/CH/DH/BH; a REX byte (even an
  otherwise-empty `0x40`) switches those same field values over to
  meaning SPL/BPL/SIL/DIL instead. This port's register model has no
  separate AH/BH/CH/DH token (see `Ast_asm6.ml`'s `width` comment), so
  whenever the grammar names SP/BP/SI/DI as a byte-width *register
  value*, a REX byte must be forced -- confirmed against real 6a/6l:
  "MOVB $6,SI" -> `40 b6 06`. Crucially, this only applies to a
  register *value* (ModRM.reg, or ModRM.rm at mod=11), never to the
  *same* register used as a memory addressing base (mod!=11 -- no
  high-byte ambiguity exists for an address) and never to a fixed
  opcode-extension digit that happens to also fall in 4-7 (SUB's ext=5,
  XOR's ext=6, CMP's ext=7) -- see "Real bugs/quirks" below for the
  real, differential-testing-caught bug from conflating these.
- `Lea` (LEAQ, address-of-global only, 64-bit only): goken's own
  "built-in LEAQ" `Zaut_r` row, opcode `0x8d`, always the
  absolute-disp32-via-SIB addressing shape (goken's non-PIE amd64
  default -- confirmed `HEADTYPE`-gated in `span.c`'s `asmandsz()`,
  macOS PIE uses RIP-relative instead, not implemented here).
- `Call` (direct only, to a label): opcode `0xe8` + rel32. Always
  exactly 5 bytes regardless of the actual displacement (unlike ARM's
  own branch-range story), so no chicken-and-egg sizing problem here.
- `Cmp` (CMPQ/CMPL/CMPW/CMPB, immediate or register): goken's
  `ycmpl`/`ycmpb`-shaped compare, same operand-role-order quirk
  documented in `Ast_asm6.ml`'s own `Cmp` comment (the ModRM r/m
  operand is the *first* written operand here, unlike `Arith`).
  Immediate form wired for signed-8-bit (Q/L/W, `Zm_ibo`/`0x83 /7`) or
  the full unsigned byte range (B, `0x80 /7`, same "no wider form"
  reasoning as `Arith`); register form via `Zm_r` (`0x39` for Q/L/W,
  `0x38` for B -- same "B is one less" pattern) only (not the
  reverse-direction `Zr_m`/`0x3b`/`0x3a` row). No `Yi0`/`Zclr` row
  exists for CMP in goken's own `ycmpl`/`ycmpb` (unlike MOVQ/MOVL/
  MOVW), so no special-casing needed there. B has its own AL-special-
  case mirror of `Arith`'s: `ycmpb`'s own `Yal,Yi32,Z_ib` row comes
  *before* the general ModRM row (the *opposite* order from
  `ycmpl`'s), so "CMPB AX,$imm" always takes the opcode-alone `0x3c`
  form -- confirmed against real 6a/6l: "CMPB AX,$5" -> `3c 05`.
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
- The static/local symbol `foo<>` suffix: already plumbed through the
  shared `Lexer_asm.mll`/`Parser_asm.mk_g` machinery (the same TLT/TGT
  fix the ARM port added), and this arch's own grammar copied the
  `name`/`pointer` rule for it from the start -- now actually
  exercised and confirmed byte-identical (`static_symbol.s`, LEAQ of a
  `<>`-scoped `DATA`/`GLOBL`). `NAME = value` constant definitions are
  a separate, real, *cross-arch* limitation (`Parser_asm.ml`'s own
  header comment: deliberately unimplemented everywhere, not amd64-
  specific) -- still not wired, see [[hello-libc-integration-test]]
  for the same gap found on ARM.
- `Move`'s immediate form to a **register** also handles a true 64-bit
  immediate now (goken's `Yi64`/`Ziq_rp` row, opcode `0xb8+reg` with
  REX.W and a full 8-byte immediate) -- reached whenever the value
  doesn't fit the `Ys32` class the `0xc7` form above already covers.
  Real amd64 has no memory-destination equivalent (an 8-byte immediate
  can't be encoded as a MOV operand to memory at all), so that's not a
  gap, just architecturally impossible.
- **Floating point (single- and double-precision SSE, no x87)**:
  `MovF` (MOVSD/MOVSS, register<->register/memory), `ArithF`
  (ADDSD/SUBSD/MULSD/DIVSD and their SS-suffixed siblings, real x86's
  own 2-operand in-place shape), `CmpF` (UCOMISD/UCOMISS, sets integer
  EFLAGS -- this port's *existing* unsigned `Jcc` conditions are reused
  as-is for a float branch, no new condition type needed), `CvtIntToF`/
  `CvtFToInt` (CVTSQ2SD/CVTSQ2SS and CVTTSD2SQ/CVTTSS2SQ, 64-bit-
  int<->float, truncating on the float-to-int direction). Precision is
  threaded as `A.floatp_precision` alongside each opcode (matching
  Ast_asmi.ml/RISC-V's own `ArithF of (arithf_opcode *
  A.floatp_precision) * ...` convention -- one AST case per
  *operation*, not one per operation-times-precision), rather than
  ARM64's own sibling-opcode-per-precision choice (`FADDS`/`FADDD`),
  since amd64's own opcode *bytes* genuinely don't change with
  precision (only the legacy prefix does -- see `sse_prefix`). The XMM
  register file (`xregister = X of int`, D_X0..D_X0+15 in goken's own
  numbering) reuses the *same* REX/ModRM encoding this whole file
  already built for GP registers (X8-X15 need REX.R/.B exactly like
  R8-R15) -- an `xgen` operand is simply coerced into the existing
  `gen` type (`gen_of_xgen`) and threaded through the unchanged
  `resolve_gen`/`encode_rm`/`rex_opt` machinery, rather than
  duplicating it for a second register file. Three real, non-obvious
  quirks confirmed against real 6a/6l (see "Real bugs/quirks" below):
  MOVSD/MOVSS's own reg-reg move always picks the *load* opcode (the
  opposite row-order choice from every integer `Move`), and
  UCOMISD/UCOMISS between them cover *three different* prefix stories
  (`0xf2`/Pf2 for every other double-precision instruction here,
  `0xf3`/Pf3 for single, `0x66`/Pe for UCOMISD, and *no* prefix at all
  for UCOMISS). No float immediates (real amd64 has none at all --
  confirmed "MOVSD $0,X0" is rejected outright by real 6a/6l), no x87.
- **AND/OR**: fold directly into the existing `Arith`/`arith_ext`/
  `arith_rr_opcode` infrastructure -- goken's own `yxorl`/`yxorb`
  tables are shared *verbatim* across ADD/SUB/XOR/AND/OR (confirmed
  reading `optab.c`: `AANDL`/`AORL`/etc list the exact same `yxorl`
  y-table their ADD/SUB/XOR siblings do), so this needed no new
  encoding logic at all, just two more `arith_ext`/`arith_rr_opcode`
  table entries (and the B_-width AL-special-case formula,
  `(ext<<3)|0x04`, already generalized correctly with zero code
  changes -- confirmed against real 6a/6l: "ANDB $5,AX" -> `24 05`).
- **`Shift`** (SHL/SAL -- real aliases, same opcode -- SHR, SAR): a new
  instruction family, goken's own `yshl`/`yshb` tables (optab.c). Three
  genuinely different real-x86 encodings share one AST case: shift-by-
  *literal* `1` (goken's own `Yi1` class, opcode alone, no immediate
  byte at all -- confirmed "SHLQ $1,AX" -> `48 d1 e0`, vs "SHLQ $4,AX"
  -> `48 c1 e0 04`), shift-by-immediate-N (opcode+ModRM+1-byte
  immediate, regardless of width -- real x86 shift counts are always
  masked to 5 or 6 bits, so there's no wider immediate form the way
  arithmetic has), and shift-by-CL/CX (opcode+ModRM, amount implicit --
  goken's own y-table has no general-register row, only `Ycl`/`Ycx`,
  confirmed any other register fails at `6l` with "notfound", guarded
  here the same way). Same B_-is-one-less opcode pattern as every
  other arith-family instruction in this file.
- **The `0x81`/imm32 arith form, finally wired** (Arith's own `Zilo_m`
  and Cmp's own `Zm_ilo`) -- deferred since the very first checkpoint,
  landed this batch the moment a realistic AND-mask (`$0xFF`, outside
  imm8's signed range) needed it. Comes with its own AX-implicit-
  opcode special case mirroring the byte-width AL one from an earlier
  checkpoint, one level up: goken's own `Yax,Yi32,Zil_`/`Z_il` rows
  (opcode family `(ext<<3)|0x05` -- e.g. ADD=`0x05`, AND=`0x25`,
  CMP=`0x3d` -- confirmed against real 6a/6l: "ANDL $0xFF,AX" ->
  `25 ff 00 00 00`, "CMPQ AX,$-1000" -> `48 3d 18 fc ff ff`), reached
  whenever the destination/compared-register is exactly AX and the
  immediate doesn't already fit the narrower imm8 row; any other
  destination falls through to the general ModRM `0x81` form. W_'s own
  immediate is 2 bytes in both rows, not 4 (confirmed "ANDW
  $0x1234,AX" -> `66 25 34 12`).
- **`Extend`** (MOVBLSX/MOVBLZX/MOVBQSX/MOVBQZX/MOVWLSX/MOVWLZX/
  MOVWQSX/MOVWQZX/MOVLQSX/MOVLQZX) -- goken's `ymb_rl`/`yml_rl`-shaped
  sign/zero-extending "widening move", real x86's own `MOVSX`/`MOVZX`/
  `MOVSXD` opcodes. Byte-sized sources need the same byte-register
  REX-forcing quirk B_-width arithmetic already established (goken's
  own `Zmb_r` case calls `bytereg()` on the source, confirmed
  "MOVBLZX SI,DX" -> `40 0f b6 d6`, an otherwise-empty REX forced to
  select SIL); word/long sources don't. `MOVLQZX` looked at first like
  a trivial alias for plain `MOVL` (goken's own table entry really is
  just the ordinary `0x8b` load opcode, since a 32-bit register write
  already implicitly zero-extends to 64 bits in real amd64) -- but its
  own table has *only* the load-direction row, while `Move`'s own
  reg-reg case picks the *store* direction first (see `Move`'s
  comment), so aliasing it onto `Move` directly produced the wrong
  bytes for the reg-reg case specifically (`89 d8` instead of goken's
  own `8b d8`) -- caught immediately by testing the actual reg-reg
  shape, not assumed correct from the "it's just MOVL" reasoning
  alone. Routed through `Extend`'s own dedicated (always-load-
  direction) codegen instead.
- **`Unary`** (NEG/NOT/INC/DEC) -- goken's `yincb`/`yincl`/`yincw`/
  `yscond`-shaped single-operand ModRM-extension-group op (`Zo_m`, the
  same Z-code `Shift`'s own shift-by-1/shift-by-CL cases already use:
  opcode + ModRM with a fixed extension digit, no immediate at all).
  Same B_-is-one-less opcode pattern as every other arith-family
  instruction here (NEG/NOT: `0xf6` vs `0xf7`; INC/DEC: `0xfe` vs
  `0xff`).
- **`MulDiv`** (single-operand MUL/IMUL/DIV/IDIV, goken's `ydivl`/
  `ydivb` tables, implicit AX(:DX)) and **`Imul2`** (IMUL's own
  2-operand form, goken's `yimul` table, opcode `0x0f 0xaf`,
  ModRM.reg=dst/ModRM.rm=src -- the same load-shaped role assignment
  `Move`'s own load clause uses). `IMULQ`/`IMULL`/`IMULW`'s own
  mnemonic is shared between both AST shapes, disambiguated purely by
  operand *count* (grammar lookahead past `gen`, matching how real 6a
  itself disambiguates) -- `IMULB` only ever has the single-operand
  form (real amd64's own `0x0f 0xaf` destination is always a full
  register, never `Yrb`). Plus `Cwd`/`Cdq`/`Cqo` (real x86's own fixed
  `0x99` opcode, prefix-selected by width -- the standard sign-extend-
  AX-into-DX:AX prep step before a signed divide, confirmed used
  directly ahead of `IDIVL` in real 6c output).

Deliberately not wired (all raise `Todo` rather than emit wrong
bytes): any memory
base register other than SP (BP/R13 need a real ModRM/SIB special case
for `[rip+disp32]` this port doesn't have -- also, unrelatedly, SP
itself can't be used as a byte-width *register value* at all, not even
by goken -- see "Real bugs/quirks" below), indexed addressing
(SIB.index, hence REX.X), x87, indirect CALL/JMP *through memory*
(only through a register is wired), Jcc/Jmp near-form relaxation,
legacy AH/BH/CH/DH byte-register forms (this
port's register model has no token for them -- see `Ast_asm6.ml`'s
`width` comment).

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
- **A real gap in goken itself: SP cannot be used as a byte-width
  register value at all, not even by real 6a/6l** -- `span.c`'s own
  `oclass()` has a `case D_BPB: case D_SIB: case D_DIB: ...` block
  covering BP/SI/DI's byte forms, but the parallel `case D_SPB:` line
  is *commented out* in this vanilla-imported goken source. Confirmed
  empirically: "MOVB $5,SP" assembles fine under `6a` (which doesn't
  itself encode instructions, just parses -- see this whole project's
  own 6a/6l split) but fails at `6l`'s own `doasm()` with `doasm:
  notfound from=... to=... MOVB $5,SPB` -- the row search can't
  classify `D_SPB` at all, so no y-table row ever matches, for *any*
  immediate value or opcode. BP/SI/DI/R8-R15 all work fine (confirmed
  side by side in the same test file) -- this is specific to SP. Since
  there's no goken reference output to match against, this port simply
  doesn't need to (and doesn't) support it either; `movb_arith.s`
  exercises SI/DI instead for the REX-forcing story.
- **A real bug in this port's own first attempt at the REX-forcing
  quirk above**: applied `regrex_forces_rex` to `reg_field`
  unconditionally, without checking whether `reg_field` was actually a
  *register* -- broke two real cases the moment a fixture exercised
  them: "SUBB $1,BX"/"CMPB BX,$5" (immediate-group opcodes, where
  `reg_field` is really just a fixed extension digit that happens to
  numerically land in 4-7 for SUB/XOR/CMP -- real 6a/6l: `80 eb 01`/
  `80 fb 05`, no REX at all) and "MOVB AX,-8(SP)" (SP used as a *memory
  addressing base*, `rm = RMem`, not a register value -- real 6a/6l:
  `88 44 24 f8`, no REX). Fixed by adding an explicit `reg_is_register`
  parameter to `rex_opt` (the two immediate-group `Arith`/`Cmp` B_
  clauses pass `false`; every reg-reg/move clause, where `reg_field` is
  always a genuine register, passes `true`) and narrowing the `rm`-side
  check to `RReg` only (dropping the `RMem` case it previously shared
  with `rex_b_of_resolved_gen`, which legitimately does care about
  R8-R15-as-a-memory-base for an unrelated reason -- REX.B -- and so
  correctly keeps its own `RMem` case). Caught immediately by
  `movb_arith.s`'s own byte comparison before this checkpoint's commit,
  not after.
- **A real, non-obvious row-order asymmetry between `ymovq`/`ymovl`
  (integer MOV) and `yxmov` (MOVSD)**: `ymovl`'s own table checks the
  *store* row (`Zr_m`, opcode `0x89`) before the *load* row (`Zm_r`,
  `0x8b`), so this port's `Move` deliberately matches its store clause
  first (see `Move`'s own comment) -- `yxmov` lists them in the
  *opposite* order (`Zm_r_xm`/load first), confirmed against real
  6a/6l: "MOVSD X0,X1" (a plain register-to-register move, where both
  rows would otherwise apply) -> `f2 0f 10 c8`, the *load* opcode, not
  `0x11`. Caught by checking real byte output *before* writing
  `MovF`'s own codegen clauses, not as a bug fix afterward -- worth
  recording since assuming "MOV-shaped instructions all order their
  y-table rows the same way" would have produced a fixture that failed
  only on the specific reg-reg case, not the reg-mem ones (which agree
  either way, since only one row can ever match when memory is
  involved).
- **Confirmed UCOMISD's own prefix in goken's real `optab.c` is `Pe`
  (0x66), not `Pf2`** (0xf2) -- every *other* SSE instruction wired
  this checkpoint (MOVSD, ADDSD/SUBSD/MULSD/DIVSD, CVTSQ2SD/CVTTSD2SQ)
  uses `Pf2`, so this was checked explicitly rather than assumed by
  analogy; confirmed against real 6a/6l byte output ("UCOMISD X1,X0"
  -> `66 0f 2e c1`, no `f2` byte anywhere in the instruction).
- **Confirmed real amd64 (and real 6a/6l) has no float-immediate
  encoding at all**: "MOVSD $0,X0" is accepted by `6a`'s own parser but
  rejected by `6l`'s `doasm()` with `notfound`, the same "6a parses,
  6l encodes" split already seen for the SP-byte-register gap above --
  matching every other arch's own choice to skip float immediates
  entirely (e.g. ARM64's `Ast_asm7.ml` `FArith` comment: goken's own
  reference implementation has the exact same feature as literal dead
  code). This port's own float values are always built via
  `CvtIntToF` from an integer instead, confirmed working end-to-end in
  `float_sd.s`.
- **UCOMISS has no legacy prefix byte at all** -- confirmed against
  both `optab.c` (`{ AUCOMISS, yxcmp, Pm, 0x2e }` -- `Pm` is just the
  `0x0f` opcode-escape byte itself, not a real prefix) and real 6a/6l
  ("UCOMISS X1,X0" -> `0f 2e c1`, vs UCOMISD's own `66 0f 2e c1`) --
  checked explicitly before assuming "swap `Pf2` for `Pf3` everywhere"
  would cover UCOMISD -> UCOMISS the same way it covers every other
  SSE instruction here, since UCOMISD's own prefix (`Pe`/0x66) already
  wasn't the `Pf2` pattern to begin with.
- **Prioritized this checkpoint's own instruction picks by grepping
  real 6c-compiled output**, rather than guessing what's "essential"
  in the abstract: `6c -S` (goken's own C compiler, `-S` prints its
  goken-assembly output -- not reassemblable, same caveat as
  [[hello-libc-integration-test]]'s own `5c -S` finding, but perfectly
  readable for mnemonic *names*) against several of `tests/c/*`'s own
  `.c` files (`float.c`, `vlrt.c`, `helloprintf.c`, `rune.c`, `test.c`)
  and tallying mnemonic frequency found real gaps this port's own
  "most essential instructions" judgment call had missed: AND/OR (52
  combined occurrences), SHR/SAL/SAR (67 combined), and the general
  imm32 arith form itself (needed the moment an AND-mask like `$0xFF`
  or `$2047` -- both real, observed in `vlrt.c`/`isNaN` -- didn't fit
  imm8). Also surfaced from real amd64 assembly files elsewhere in
  goken's own tree (not 6c-generated, hand-written): `tests/s/
  regressions/amd64_psllq.s` uses `PSLLQ $imm,X` (an SSE2 *integer*
  shift on an XMM register, a different instruction family from the
  scalar-float SSE work above -- not wired yet) and a raw GP<->XMM
  `MOVQ` (bit-reinterpretation, not a float *conversion* -- also not
  wired yet, see "Suggested phase plan" below); goken's own `6c`
  itself avoids this and instead round-trips such bit-reinterpretation
  through memory (`float.c`'s own `float64tobits`: `MOVSD X0,u+-8(SP)`
  then `MOVQ u+-8(SP),AX`), already fully supported by this port's
  existing `MovF`/`Move`.

- **A real gap found while testing `Extend`, fixed the next checkpoint:
  `Move`'s own Q_/L_/W_ immediate-to-register/memory range guards were
  too narrow.** Goken's own `oclass()` classifies a *positive* hex
  literal like "$0xFFFFFFF6" (parsed as the literal value 4294967286,
  not -10) as its own `Yi32` class -- distinct from `Ys32` (sign-
  extendable 32-bit) purely by *how the value was written*, not its
  final bit pattern -- confirmed reading `span.c`'s `oclass()`: `l =
  v; if((vlong)l == v) return Ys32; if((v>>32)==0) return Yi32;` (`l`
  is a 32-bit local, so this checks "does truncating-then-sign-
  extending round-trip" for `Ys32`, then falls back to "does it fit as
  a plain 32-bit pattern, zero-extended" for `Yi32`). For a *register*
  destination this reaches `Ziq_rp`'s own case body (span.c), which
  has an internal `l = v>>32; if(l==0){ clear REX.W; emit 0xb8+reg;
  put4(v); }` downgrade -- confirmed against real 6a/6l: "MOVQ
  $0xFFFFFFF6,AX" -> `b8 f6 ff ff ff`, no REX at all, *not* the
  REX.W+0xc7 form this port's own earlier Ys32-only guard produced for
  the bit-pattern-identical "$-10". This port's earlier claim (in an
  even older revision of this doc) that Ziq_rp's own internal special
  cases were "provably unreachable for ymovq" turns out to only hold
  for the *other* internal branch (`l==-1`, sign-extending); the
  `l==0` branch is genuinely reachable. Fixed with a shared
  `fits_yi32` range check (`-0x8000_0000..0xFFFF_FFFF`) replacing the
  narrower `Ys32`-only guards throughout `Move`'s own Q_/L_/W_
  immediate clauses -- L_/W_ needed no encoding change at all (`ymovl`/
  `ymovw` have no `Ys32`/`Yi32` split to begin with, confirmed: "MOVL
  $0xFFFFFFF6,BX" -> `bb f6 ff ff ff`, same opcode family regardless;
  "MOVW $0x12345678,CX" -> `66 b9 78 56`, truncating even a value far
  exceeding 32 bits down to its own low 16 bits). Along the way, found
  a genuinely surprising real semantic quirk worth its own note: the
  *same* Yi32 immediate produces different 64-bit *values* depending
  on the destination -- register (`Ziq_rp`'s downgrade) zero-extends,
  memory (`Zilo_m`) sign-extends -- confirmed and pinned down by
  `imm_yi32.s`'s own comparison (`$0xFFFFFFF6` to a register vs to
  memory-then-reloaded genuinely differ, matching real amd64's own
  ordinary-32-bit-write-zero-extends vs MOV-r/m64-imm32-sign-extends
  semantics).

## Suggested phase plan (next checkpoints)

Roughly in the order a next real fixture would need them, mirroring
how ARM32/ARM64's own follow-up phases were sequenced:
1. `NAME = value` constants -- a real, cross-arch limitation, not
   amd64-specific, see [[hello-libc-integration-test]].
2. A real fixture for `Jmp` itself (still not covered, per "What's
   covered" above) and Jcc/Jmp's near-form relaxation, and
   memory-indirect (not just register-indirect) CALL/JMP -- see "Real
   bugs/quirks" above for why the first two are genuinely harder than
   they looked (goken's own dead-code elision and loop rotation, and
   real multi-pass distance-dependent sizing, respectively).
3. A raw GP<->XMM `MOVQ` (bit-reinterpretation, goken's own `yxmovq`-
   shaped `0x66 REX.W 0F 6E`/`0F 7E` -- confirmed real, used directly
   in `tests/s/regressions/amd64_psllq.s`, though goken's own `6c`
   itself always avoids it via a round-trip through memory instead,
   see "Real bugs/quirks" above) and SSE2 packed-integer instructions
   like `PSLLQ` (a real, if lower-priority, separate instruction
   family from the scalar-float SSE work already landed).
4. RIP-relative addressing (needed the moment a fixture targets a
   non-Linux `HEADTYPE`, or if this project ever wants position-
   independent amd64 output), indexed addressing (SIB.index/REX.X),
   and the BP/R13 ModRM special case (note: BP still isn't even wired
   in this arch's own grammar as a plain register token yet -- only
   SP/AX/CX/DX/BX/SI/DI/R8-R15 are, see `Parse_asm6.ml`).
5. x87 -- single- and double-precision SSE both landed across earlier
   checkpoints; x87 is deferred indefinitely absent a concrete need
   (real amd64 userspace code essentially never uses it; SSE is the
   real ABI convention). The 32-bit-int forms of the int<->float
   conversions (CVTSL2SD/CVTSL2SS/CVTTSD2SL/CVTTSS2SL, no REX.W) also
   aren't wired -- every GP register in this port's own scope is
   already treated as 64-bit-wide by convention, see `width`'s own
   `Q_` case -- add them if a fixture ever genuinely needs a 32-bit
   int<->float round trip.
