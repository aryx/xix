# Porting the ARM toolchain (o5a/o5l) against goken, byte-equal

Status: **complete**. Every `linkers/5l/codegen.c` case (1-15, 17,
20-21, 30-31, 35-41, 50-58, 70-76, plus MCR/MRC which isn't
case-numbered at all) is either ported, confirmed to need no new
code, or confirmed dead/unreachable in goken itself (or gated behind
a linker flag this harness never uses). See "Open issues" at the end
for the one thing still genuinely unresolved.

## Goal

Automate porting the ARM linker/assembler (o5a/o5l) against the
goken reference (`~/goken`, a C Plan 9 toolchain fork), driven by
differential testing: assemble+link the same `.s` with both goken's
5a/5l and xix's o5a/o5l, and require the final executables to be
byte-identical. Scope is ARM assembler + linker only, permanently --
not phase-1-of-N. The compiler is explicitly NOT part of this
byte-equal effort (see "Compiler is out of scope" below); there is
no planned phase 2 that extends this harness to o5c vs goken's 5c.

This is the forward-looking counterpart, for the OCaml port, of what
goken's own `docs/claude_notes/notes_arch_arm.txt` did for
reconciling two *C* lineages (5a/5c/5l vs 5ak/5ck/5lk) byte-for-byte.
Same kind of problem, same kind of playbook, different target
language.

Same methodology later extended, arch by arch: RISC-V32/64 (`oil`/
`ojl`, see `notes_riscv_port_plan.txt`), then MIPS (`ova`/`ovl`, see
`mips_port.md`) -- both docs point back here for the shared harness
shape, syncweb rules, where-changes-land guidance, and goken-flag
conventions, so read this one first even when working on another
arch.

## Grounding: how goken maps onto xix

goken's `linkers/5l/` was itself split into `codegen.c`, `datagen.c`,
`layout.c`, `profile.c`, `debugging.c`, `dynamic.c`, `hist.c`, `io.c`,
`lib.c`, `main.c`, etc. This split is exactly what xix's `linker/`
module layout mirrors (`Codegen5.ml`, `Datagen.ml`, `Layout5.ml`,
`Profile.ml`, ...), which confirms `goken/linkers/5l/codegen.c` is
the right C reference for the `(* case N: ... *)` comments in
`Codegen5.ml` -- it has the numbered asmout-style switch (~70
cases), e.g.:

    case 34: /* mov $lacon,R -> LDR x(R15), R11; ADD R11, R13, R */

goken's own reference binaries are already built at
`~/goken/ROOT/arch/boot-gcc/bin/{5a,5l}` (invoke directly, no rebuild
needed unless goken's C source itself changes).

`xix/linker/TOPORT/5l/` and `xix/assembler/TOPORT/5a/` hold an
*older* snapshot of the same C sources (predates goken's file-split
reorg -- e.g. `TOPORT/5l/asm.c` is still the pre-split monolith).
goken is the more up-to-date and better-organized reference to port
from and diff against; treat `TOPORT/` as historical/secondary.

Note there's no optab/oplook table in the OCaml port ("ocaml: No
need for optab/oplook/ocmp/cmp as in 5l. Just use pattern matching!")
-- porting a case means adding a pattern-match arm in `Codegen5.ml`,
not filling a table row. Each ported case is tagged in the code with
`(* case N: <verbatim C comment> *)`, at both the outer dispatch and
any inner sub-match (e.g. the store vs load sub-arm).

## Harness

`scripts/diff-arm.sh`, driven over the baseline corpus by
`test-arm.sh` at the repo root: a plain bash script, NOT a Testo
module/test. Testo is reserved for this project's actual correctness
test suite (`make test`), which must run without any external
toolchain installed; this harness has a hard dependency on a locally
built goken checkout and is a development/porting tool, not a CI
test. goken's location comes from an env var (`GOKEN_ROOT`, default
`~/goken`); tests skip (not fail) when it's unset/missing so `make
test` stays green without goken installed.

Per test file:

    goken: 5a -o t.5 t.s && 5l -H7 -E _start -s -o t.out.goken t.5
    xix:   o5a -o t.o5 t.s && o5l -E _start -o t.out.xix t.o5
    cmp -l t.out.goken t.out.xix   (report byte offsets, not just
                                    pass/fail -- needed to debug which
                                    case regressed)
    qemu-arm ./t.out.{goken,xix}   (functional smoke test, run even
                                    when `cmp` already disagrees,
                                    since it can point more directly
                                    at "structural" vs "wrong
                                    instruction" bugs)

Three things that shaped the harness, all resolved:

1. Object file format is intentionally NOT shared between goken and
   xix. `assembler/objects/Object_file.ml` uses OCaml Marshal
   (`.o5`/`.ov` extension) instead of replicating Plan 9's on-disk
   object format -- a deliberate simplification (see
   `assembler/CLI.ml`'s prelude). Consequence: never try to diff or
   interop `.5` vs `.o5` files. Only final linked executables are
   ever compared.
2. goken's 5l defaults to the Plan 9 native "a.out" header
   (`HEADTYPE` `H_PLAN9=2`); ELF is `H_ELF=7`, selected with `5l
   -H7`. xix's o5l always emits ELF, so the harness passes `-H7` on
   the goken side, plus `-s` to strip goken's native Plan9
   symbol/debug table (embeds the invocation cwd, not
   byte-reproducible, and xix never emits one anyway).
3. This machine is arm64 with qemu-arm installed, so 32-bit ARM
   Linux ELF binaries can actually be *run*, not just byte-compared
   -- a stronger, cheaper first signal than `cmp`, and a first-class
   step of the harness rather than an afterthought.

Given (3), fixtures use a real Linux hello-world/exit shape (real
`write`/`exit` syscalls, `SWI $0`, syscall number in R7 -- the
standard Linux ARM EABI convention), not `tests/linker/helloworld.s`
(Plan 9's native EXITS/PWRITE syscalls, only meaningful with `-H2`).
All fixtures define `TEXT _start`, not the linkers' default entry
symbol `_main`, so both sides need `-E _start` explicitly (note
OCaml's `Arg` wants `-E _start` as two argv words, not glued as
`-E_start` the way goken's getopt-style parsing tolerates).

Real bugs the very first fixture (`hello_linux.s`) surfaced before
any per-case codegen work even started:

- **ELF layout parity**: goken's build ran under qemu-arm and xix's
  crashed (SIGILL) with a 176-byte size gap and the first `cmp -l`
  mismatch inside the ELF header/program-header area. Root cause:
  missing 3rd (NOPTYPE/symtab-placeholder) program header and the
  `.text`/`.data`/`.strtab` section header table + string table
  (ported from goken's `linkers/8l/elf.c`, every constant traced, not
  guessed -- first attempt got the string table 1 byte short, caught
  by `exit_linux.s`'s no-data-section fixture). Fixed in
  `Elf.ml`/`Elf.mli`/`Execgen.ml`.
- **o5l's exit-code propagation**: verified NOT actually broken --
  `CLI.ml`'s error handler already catches `Failure s` (what `error
  node`/`failwith` raise) and returns a non-zero exit; only a
  genuinely uncaught exception (e.g. `Match_failure`) would slip
  through, and that already bubbles out to OCaml's own non-zero
  default exit.
- **`hello_linux.s` still SIGILL'd under qemu-arm** even after ELF
  parity: root cause was `linker/Flags.ml`'s `kencc_compatible`
  defaulting to `true`, which sets a MOV instruction's Rn field to
  the destination register instead of 0 -- goken always encodes
  Rn=0 for MOV, and qemu-arm's decoder SIGILLs on the Rn=rt encoding
  (verified by hex-patching just those 3 bytes in an otherwise-
  crashing binary). Fixed by flipping the default to `false`.

## Test corpus sources

The diff fixtures live under `tests/linker/arm_diff/`: each one is a
single object file (one `TEXT`, assembled and laid out directly), so
what's actually exercised is mostly assembler codegen + minimal
exec-layout, not linking proper (no cross-object symbol resolution,
no archives) -- still filed under `tests/linker/` since it's
o5a+o5l's combined output being compared, with sibling
`tests/linker/mips_diff/`/RISC-V equivalents once those arches got
the same treatment. See `tests/linker/README.md` for the fixture-
naming/`_check` conventions.

Beyond `tests/linker/`'s other fixtures (`hello.s`, `world.s`,
`helloworld.s` -- Plan 9-native, kept where they are, unrelated to
this differential effort), goken ships its own ARM assembly fixtures
at `~/goken/tests/s/mini/` (and `tests/s/{exit,features,float,
hello_arch,regressions,variants}/` for the wider arch set) -- several
fixtures here were seeded from those instead of written from scratch
(`hello_linux.s`, `exit_linux.s`, `addr.s`, `call.s`). They're
already known-good inputs to the C reference, and goken's
`tests/s/variants/` in particular are curated regression tests for
specific past bugs, which makes them good fixtures on our side too.
Prefer ones using real Linux syscalls with a Linux-friendly entry
point, so qemu-arm can double-check them too; the Plan-9-native ones
can't be run under qemu-arm on this Linux host.

Also: `cmp`-clean isn't a strong enough check on its own for a
fixture with no data section. `exit_linux.s` (no GLOBL/DATA at all)
caught a 1-byte size bug in `Elf.ml`'s shstrtab content that
`hello_linux.s` could not have caught: that fixture's section-
header/string-table region sits in the zero-padded gap before its
data segment, so a byte too few there just disappears into
already-zero padding instead of changing the file's total size. Keep
at least one no-data-section fixture in the corpus for that reason.

## Syncweb: hard constraint, both repos

Both xix and goken source files are synced to Noweb literate-
programming docs via `(*s: ... *)` / `(*e: ... *)` / `(*x: ... *)`
comments (`/*s: ... */` etc. in goken's C). Never add, remove, or
modify these markers in either codebase during this work, in either
direction. Concretely:
- Editing the *body* of an existing chunk (the code between an
  existing s:/e: pair) is fine -- that's how tracked code normally
  evolves.
- Do NOT introduce brand-new `(*s: ...*)`/`(*e: ...*)` wrappers for
  new functions/types added while porting; write new code as plain
  OCaml (or plain C on the goken side) without inventing chunk
  names, and leave wiring it into the literate docs to the project
  owner.
- Never rename, move, or delete an existing marker line, even when
  refactoring the code it wraps.

## Where changes land: xix vs goken

The bulk of this effort is xix-side porting/fixes (`Codegen5.ml`,
`Layout5.ml`, the harness itself, exit-code handling, etc.) -- goken
is the reference and should stay stable by default.

It is fine, though, to make small, reasonable, flag-gated
adjustments to goken itself when they help reach a byte-matching
baseline faster -- e.g. a flag to disable an optimization pass,
suppress extra debug info, or otherwise simplify its output so the
comparison isolates the construct actually being ported rather than
an unrelated goken-side knob. Keep such changes minimal, opt-in (a
new flag, not a changed default), and still fully respecting goken's
own syncweb markers under the same rule as above. When in doubt,
prefer solving it on the xix side instead.

## Compiler is out of scope

occ (xix's C compiler) is *not* going to be ported/kept byte-equal
against goken's per-arch compilers (5c/6c/7c/8c/vc/ic/...). goken
duplicates a lot of codegen logic across those per-arch compilers;
the actual plan for occ is to instead reuse `~/c--`'s backend
architecture (a fork of Quick C--, at `~/github/fork-c--`, with a
proper shared arch/backend split instead of one-file-per-arch
duplication). So this differential-testing/byte-equal methodology
stays scoped to the assembler+linker (o5a/o5l vs goken's 5a/5l); it
is not meant to be extended to the compiler later, and o5c vs 5c
byte-equality is explicitly not a goal.

## Port log

Case numbers refer to `linkers/5l/codegen.c`'s `switch` on `o->type`.

### Phase 0 -- harness

- **diff harness script** (assemble+link both sides, `cmp -l`,
  qemu-arm): `scripts/diff-arm.sh` + `test-arm.sh`, see "Harness"
  above for the recipe and the real bugs it surfaced (ELF layout
  parity, the MOV-Rn `kencc_compatible` SIGILL).
- **o5l exit-code propagation on `error node "TODO: ..."`**: verified
  not actually broken (see "Harness" above).

### Phase 0.5 -- baseline corpus (constructs already ported)

- **`immrot` + `offset_to_R12` (address-of-global)**: both were
  already-flagged stubs ("less: x - BIG at some point", "TODO: port
  full code of immrot") -- finished properly rather than patched
  around, since a real immrot (full 8-bit-rotated-immediate search)
  is needed generally, not just for one case. Ported goken's actual
  algorithm (`5l/span.c` `immrot()`: try each of the 16 even
  rotations, unsigned 32-bit arithmetic) and the BIG bias
  (`(1<<12)-4`, `5l/l.h`) that `offset_to_R12` was missing. Caught by
  `kitchen_sink.s` using two GLOBLs: with only one global at offset 0
  (every earlier fixture), xix's old "offset=0 -> pool, else fast ADD
  path" happened to agree with goken by coincidence; a second global
  at a nonzero offset exposed that goken's real condition is
  "immrot(offset-BIG) succeeds and isn't exactly 0", essentially
  never true for a small data segment (BIG is meant for symbols
  positioned near it in a *large* segment) -- so goken pool-loads
  both, while xix's old code fast-pathed the nonzero one.
- **`kitchen_sink.s`** (hand-written, not from goken): combines
  ADD/SUB/MUL/SLL, a BL/RET local call, address-of-global, and
  write/exit in one straight-line program -- broader coverage per
  fixture. Writing it caught the immrot/BIG bug above, then a
  separate, distinct finding once fixed: an earlier version used
  CMP+BEQ to pick between two exit messages, and goken turned out to
  duplicate the short exit epilogue (`mov r7,#1; svc #0; pop {pc}`)
  into *both* branches instead of branching to a shared tail --
  something in 5a/5l's asm/noop layer decides to duplicate short
  tails instead of always branching. Real, but distinct from
  anything in the case list -- not chased; rewrote the fixture
  branch-free instead.
- **Corpus extension**: `exit_linux.s`, `addr.s`, `call.s`, seeded
  from goken's own `tests/s/{exit,variants}/` (see "Test corpus
  sources"). All 4 baseline fixtures: PASS byte-identical + PASS same
  qemu-arm exit code. Also fixed `diff-arm.sh`: `set -e` was aborting
  the script on a fixture's intentional non-zero exit code
  (`exit_linux.s` exits 42).
- **`hello_linux.s`: last 6 bytes closed** in `Layout5.ml`:
  implemented goken's `checkpool()`'s "true end of program" flush
  trigger (`if(p->link==P) flushpool(p,true)`) -- inserting a
  synthetic self-branch ("B .", an unreachable trap) right before the
  flushed literal pool, matching goken's always-guard-against-
  fallthrough behavior even though it's dead code here. The self-loop
  falls out naturally from `gbranch_static` reading the branch's own
  `.branch` field. Mid-function early-flush (pool > ~4KB or LDR
  offset overflow) is NOT implemented -- pools must stay small; a
  real limit, not just a decision to revisit later.
- **`hello_linux.s`: runs correctly under qemu-arm**: see the
  `kencc_compatible`/MOV-Rn bug in "Harness" above.

### Phase 1 -- codegen.c case backlog

Cases with a matching `(* case N: *)` comment early on: 1 (`op
R,[R],R`), 2 (`movbu $I,[R],R`), 3 (`add R<<[IR],[R],R`), 4 (`add
$I,[R],R`, address-of-global fast path, SB-relative), 5 (`bra s`,
both B/BL/AL and Bxx/conditional -- same optab case, only
`p->scond` differs), 6 (`b ,O(R) -> add $O,R,PC`), 7 (`bl ,O(R) ->
mov PC,link; add $O,R,PC`), 8 (`sll $c,[R],R -> mov (R<<$c),R`), 9
(`sll R,[R],R -> mov (R<<R),R`), 10 (`swi [$con]`), 14 (`movb/movh/
movhu R,R -> SLL/SRA/SRL`, NOT movbu -- that's case 58, goken's own
case-14 comment text is slightly imprecise), 15 (`mul r,[r,]r`), 20
(`mov/movb/movbu R,O(R)`, short/12-bit offset store), 21 (`mov/movbu
O(R),R`, short/12-bit offset load; Byte U only -- signed byte/
halfword load is case 22, not this), 41 (`rfe -> movm.s.w.u
0(r13),[r15]`), 58 (`movbu R,R -> AND $0xff, R, R`).

- **Case 13** `op $lcon, [R], R` -- generic Arith (AND/ORR/EOR/ADD/
  SUB/BIC/ADC/SBC/RSB/RSC/MVN/MOV, aliased via goken's `buildop()` to
  the same oprange as AADD/AMVN/ACMP) with a too-big immediate: load
  it into REGTMP via the literal pool first, then apply the real op
  using REGTMP as the "from" register (size=8: LDR + OP). Added
  `lcon_case13.s` (ADD $0x12345678,R0,R1). PASS byte-identical + PASS
  same qemu-arm exit code first try.
- **Case 4/34** address-of-local/param ($lacon) -- mirrors the
  address-of-global case 4/12 split, just SP-relative instead of
  SB-relative: fast path (fits immrot) is a single ADD, slow path is
  REGTMP via the literal pool + ADD. Reused
  `base_and_offset_of_indirect` (feeding it `Entity entity` for the
  Local/Param case) instead of duplicating its offset arithmetic.
  Caught two real, previously unverified bugs -- nothing before this
  exercised `Entity(Local)`/`Entity(Param)` at all, every earlier
  fixture used raw `Indirect(reg,off)` syntax like `4(R13)` instead:
  - `base_and_offset_of_indirect`'s "+4 for the caller/RLINK-slot"
    adjustment was on the wrong branch: Param (this codebase's name
    for the SP-mapped token) had it, Local (the FP-mapped token)
    didn't; should be the other way. Confirmed directly against goken
    with plain memory access (`MOVW x-8(FP),R1` / `MOVW x+8(SP),R1`,
    no $) before touching any $lacon code, to isolate this from the
    $lacon-specific issue below.
  - goken's `aclass()` (`span.c`) decides RACON-vs-LACON (fits-immrot
    fast path vs literal-pool slow path) using the offset *before*
    `Rewrite5.rewrite`'s own "+4 for RLINK-save" adjustment to
    autosize -- a value no longer available by the time Codegen5 runs
    (Rewrite5 bakes it permanently into the TEXT node's frame size).
    Confirmed empirically: frame=$8192, off=-8 gives a fully-adjusted
    offset of 8192 (fits immrot), but goken still emits the slow/pool
    form, because ITS classification value is 8192-8=8184 (doesn't
    fit). Fixed by reconstructing the pre-adjustment offset
    (`env.autosize - 4 + off`) purely for the fits-check, while still
    encoding the real, fully-adjusted offset in either branch. This is
    not a rare edge case -- any real program combining a round frame
    size (4096, 8192, ...) with a small local/param offset can hit
    it.
  Added `lacon_case4_12.s` (fast-path Local, fast-path Param, and a
  second slow-path instance). PASS byte-identical + PASS same
  qemu-arm exit code.
- **Case 30/31** `mov/movb/movbu R,L(R) / L(R),R` -- long (>12-bit)
  stack/SB-relative offset: load it into REGTMP via the literal pool,
  then a register-offset LDR/STR (`gmem`'s existing `Either.Right`
  path). Caught two real, previously-latent bugs while verifying
  against goken:
  - `gmem`'s `Either.Right` (register-offset) branch never set the U
    (add) bit, defaulting to SUB instead of ADD -- goken's
    `olr()`/`olrr()` always ADD when passed a register number (always
    >= 0). Nothing before case 30/31 ever exercised gmem's
    register-offset path.
  - `Layout5.ml`'s `literal_pools` is a stack (LIFO push) but was
    flushed without reversing, so a pool with 2+ simultaneous entries
    came out in reverse insertion order vs goken's FIFO
    `addpool()`/`flushpool()`. Every earlier fixture only ever had one
    pending pool entry per flush.
  Also surfaced the "literal-pool value deduplication" gap (see "Open
  issues"): `longoff_case30_31.s` deliberately uses each offset only
  once to isolate case 30/31's own correctness from that gap. PASS
  byte-identical + PASS same qemu-arm exit code.
- **Case 11** `word` -- already handled generically by the shared
  `Codegen.default_rules` (used by ARM/MIPS/RISC-V alike): AWORD is
  never hand-written (it's what 5a itself generates for a
  literal-pool entry), so this is verified indirectly by every
  fixture whose codegen falls back to a literal pool (`bigimm_case12.s`
  for case 12, `lcon_case13.s` for case 13), not by a dedicated
  fixture.
- **Case 54** floating point arith (ArithF: ADDF/SUBF/MULF/DIVF/
  ADDD/SUBD/MULD/DIVD, plus CmpF: CMPF/CMPD which share this case in
  codegen.c via `if(p->to.type==D_NONE) rt=0` -- kept as its own OCaml
  arm since `Ast_asm5.CmpF` is already a separate constructor). This
  is goken's legacy ARM 7500 FP (coprocessor 1) encoding -- defunct
  hardware, but what goken emits BY DEFAULT (`vfp = debug['f']`, off
  unless -f is passed; VFP -- cases 74-76 -- is the opt-in one).
  Float immediates are limited to goken's `chipfloats[]` (`float.c`):
  exactly {0,1,2,3,4,5,0.5,10} -- anything else is a genuine assembler
  error here, not goken's own silent `diag(); rf=0` fallback. Added
  `fpa_case54.s`. PASS byte-identical; confirmed empirically that
  qemu-arm has no FPA coprocessor at all to trap/emulate it -- both
  sides SIGILL identically (rc=132) on the very first FPA
  instruction, itself the matching signal.
- **Case 40** `swp oreg,reg,reg` -- ARM's atomic exchange (SWPW/
  SWPBU), deprecated since ARMv6 in favor of LDREX/STREX but still
  real (goken's own runtime uses it for spinlocks). The AST shape
  (`Ast_asm5.SWAP`) and grammar already existed but the SWPW/SWPBU
  keyword-table entries were never wired up in `Parse_asm5.ml` --
  added those two lines. The 2-operand grammar form (`SWPW (R4), R3`)
  is the classic atomic-exchange-in-place idiom: the same register is
  both the new value written (Rm) and the destination that receives
  the old value (Rd); the 3-operand form is implemented too but not
  independently verified against a real fixture. Added
  `swp_case40.s`.
- **MAJOR FIND, FIXED**: `immrot()` was subtly wrong for some values
  -- goken's own `immrot(ulong v)` (`5l/span.c`) is, relative to true
  ARM rotated-immediate semantics, buggy on a 64-bit host: `ulong` is
  64-bit unsigned (typedef in `include/core/types.h`), but the
  rotation trick (`v = (v<<2)|(v>>30)` up to 16 times) was written
  assuming 32-bit wraparound. On a 64-bit `ulong`, bits pushed past
  bit 63 are simply lost and `v>>30` starts pulling in bits that never
  actually wrapped around from a genuine 32-bit rotation -- so for
  some values that ARE valid ARM rotated immediates (e.g. 0x9000 =
  0x90 rotated), goken's own `immrot()` returns 0 (not encodable), and
  it takes the literal-pool slow path instead of a single MOV/ADD.
  Since the whole point of this port is byte-for-byte matching THIS
  actual goken binary (not "more correct" ARM codegen), `Codegen5.ml`'s
  immrot needed to replicate that exact 64-bit non-wrapping
  computation -- rewritten using Int64 (OCaml's native int can't
  safely hold the needed 64-bit unsigned patterns). Previously
  undetected because every earlier fixture's constants either fit
  directly or were clearly too large either way; `swp_case40.s`'s
  `MOVW $0x9000, R4` is what exposed it. Re-verified the entire
  existing corpus still passes byte-identical after the fix.
- **Case 72/73** `movh/movhu R,L(R) -> strh` / `movb/movh/movhu
  L(R),R -> ldrsb/ldrsh/ldrh` -- long (>8-bit) offset halfword/
  signed-byte store/load, same REGTMP-via-literal-pool pattern as
  case 30/31, using `ghalfword`'s new register-offset form (extended
  from immediate-only: bit22=0, offset register directly in
  bits[3:0], no magnitude limit -- ported from goken's `oshrr`/
  `olhrr`, which XOR `olhr`/`oshr`'s immediate-mode bit22 off). Added
  `halflong_case72_73.s` (each offset used exactly once, same
  literal-pool-dedup-avoidance reasoning as `longoff_case30_31.s`).
  PASS byte-identical + PASS same qemu-arm exit code.
- **Case 70** `movh/movhu R,O(R) -> strh` (real ARMv4T STRH,
  `ghalfword` helper).
- **Case 71** `movb/movh/movhu O(R),R -> ldrsb/ldrsh/ldrh` (real
  ARMv4T LDRSB/LDRSH/LDRH). Added `ghalfword` (ARM's distinct
  Load/Store-Halfword-and-Signed-Byte bit layout, split 8-bit
  immediate, SH-bit variant selector) -- caught a real bug while
  verifying against goken: the STORE side (case 70/`oshr`) never
  looks at signedness at all (there's only one STRH), only the LOAD
  side (case 71/`olhr`'s XOR based on `p->as`) does; `ghalfword`'s
  first draft wrongly varied the SH bits by signedness for STORE too,
  encoding STRH as a bogus STRD-shaped opcode -- caught by a 1-byte
  `cmp -l` diff, fixed. Added `halfword_case70_71.s`. PASS
  byte-identical + PASS same qemu-arm exit code.
- **Cases 22/23/32/33 SKIPPED** (`movb/movh/movhu` via
  byte-split-and-shift, the pre-ARMv4T fallback for O(R) and L(R)
  offsets) -- confirmed dead in practice: goken's `buildop()` only
  disables the V4-flagged optab rows (70-73, real STRH/LDRH/LDRSB/
  LDRSH) when `armv4` is false, and `armv4 = !debug['h']`
  (`5l/span.c`) -- true by default, so 70-73 always win over
  22/23/32/33 unless goken is invoked with `-h`, which this harness
  never does. Porting 22/23/32/33 would be porting code no test here
  could ever exercise or verify.
- **Case 74/75** vfp floating point arith/compare -- turns out this
  IS important in practice, not just a "-h"-style dead-code corner:
  real ARMv6+ hardware (e.g. Raspberry Pi 1's ARM1176JZF-S) and
  principia-softwarica's own kernel/userland both target VFP, not
  FPA (FPA is defunct hardware, see case 54) -- so any float-using ARM
  program built with goken's *default* settings would be
  non-functional on that real hardware, same failure mode as
  `fpa_case54.s`'s SIGILL under qemu-arm. `vfp = debug['f']`
  (`5l/span.c`) is OFF by default (opposite polarity of the armv4
  story), so this needed a new xix-side flag (`-f`, `Flags.vfp`,
  ARM-only) mirroring goken's, plus `scripts/diff-arm.sh` and
  `test-arm.sh` threading an optional extra-flags field through to
  pass `-f` identically to both 5l and o5l.
  ArithF's dyadic path (ADD_/SUB_/MUL_/DIV_) dispatches on
  `!Flags.vfp` between `gop_arithf` (FPA) and `gop_arithf_vfp` (VFP,
  `opvfprrr`) -- VFP doesn't support float immediates at all (goken
  `diag()`s on D_FCONST there) and the AST's `arithf_opcode` has no
  MOVF/MOVD/MOVFD/MOVDF (case 74's monadic move/precision-conversion
  ops), so only the dyadic path was ever reachable here anyway. CmpF
  likewise dispatches -- VFP's compare is a genuinely different shape
  from FPA's: it needs a *second* fixed instruction ("MRS APSR_nzcv,
  FPSCR", `gop_cmpf_vfp_mrs`) to move the comparison result into the
  ARM CPSR where Bxx reads flags from (size=8 vs FPA's size=4), and
  the field layout for the 2nd operand differs (bit12 vs FPA's
  bit16). Added `vfp_case74_75.s`. PASS byte-identical on the first
  try, AND -- unlike `fpa_case54.s` -- PASS genuinely correct
  execution under qemu-arm (rc=0, the CMPF/BEQ branch actually
  taken).
- **Case 55/76** floating point fix and float (int<->float
  conversion, MOVWF/MOVFW/MOVWD/MOVDW) -- added the AST/grammar work
  this needed (`Ast_asm5.MOVWF`/`MOVFW`, `Parser_asm5.mly`/
  `Parse_asm5.ml` keyword table, `Meta_ast_asm5.ml`'s hand-maintained
  `vof_instr`, `Rewrite5.ml`'s exhaustive match) since without this a
  VFP/FPA program could compute but never get a real integer value
  into or out of a float register. `Object_file.version` bumped
  (8->9). A stale `bin_dune` copy built against the old count of
  constructors segfaulted (not a catchable OCaml exception) on every
  fixture, not just the new ones, until rebuilt -- the classic
  bin_dune-staleness gotcha, just with an unusually scary-looking
  symptom this time.
  FPA (case 55, `gop_fixfloat`): direction (dirbit at bit20: 0=to
  float, 1=to int) and D-precision (bit7) vary; register placement
  differs by direction (int reg @ bit12/freg @ bit16 for ToFloat,
  freg @ bit0/int reg @ bit12 for ToInt).
  VFP (case 76) is a genuinely different shape: converting between a
  core register and a VFP register needs an extra VMOV to shuttle the
  raw bit pattern in/out first (VFP's convert instructions only
  operate within the VFP register file) -- 2 instructions each
  direction, using FREGTMP (F15/D15, matching int-side REGTMP) as the
  scratch register for the ToInt direction. Got the encoding wrong
  twice on the first attempt (caught by testing against goken
  directly): missed the `0xe<<24` base bits on the two opvfprrr-
  formula instructions, and mis-derived which hex digit encodes
  VMOV's direction bit (bits[23:20], not bits[19:16] as first
  computed, which is actually the register slot).
  Added `fixfloat_fpa_case55.s` and `fixfloat_vfp_case76.s` (both: 42
  -> float reg -> back -> exit(42)). PASS byte-identical AND PASS
  genuine functional round-trip under qemu-arm (rc=42 both sides,
  both FPA and VFP) -- FPA's arithmetic SIGILLs but apparently qemu
  does implement these two particular legacy opcodes.
- **Case 12** `movw $lcon, reg` -- both the address-of-global slow
  path (`MOVW $L(SB),RT -> LDR` from literal pool) and the plain
  too-big-immediate case (`MOVW $0x12345678,R -> LDR` from literal
  pool, reusing `Ast_asm.Int` through the same PoolOperand mechanism,
  already generically handled by `Codegen.default_rules`'s WORD
  case). Added `bigimm_case12.s`. PASS byte-identical + PASS same
  qemu-arm exit code (120, from the truncated-to-byte 0x12345678
  status value).
- **Case 16 SKIPPED** `div r,[r,]r` -- goken itself just emits an
  invalid/trap opcode here (`o1 = 0xf<<28`, the ARM "never" condition
  with a bare invalid encoding; ARMv5/v6 has no hardware integer
  divide). Porting this would mean porting a trap, not real
  functionality. Real div/mod on this target is a compiler (occ)
  concern -- runtime library calls -- which is out of scope (see
  "Compiler is out of scope").
- **Case 17** is AMULL (64-bit long multiply, register-pair result),
  not mod/div as earlier notes guessed -- optab.c has no AMOD entry
  at all -- real, reachable syntax: `"MULL cond R1,R2,(HI,LO)"` (a.y's
  `regreg: '(' regi ',' regi ')'`), all 4 sign/accumulate variants
  (MULL/MULLU/MULAL/MULALU) sharing one new instr constructor (`MULL
  of A.sign * bool * reg * reg * reg * reg`) since goken's own
  `oprrr()` encodes the 4 mnemonics as a 2-bit sub-field (sign,
  accumulate) rather than 4 unrelated opcodes. New `gmull_opcode`
  helper for that bit packing -- caught a real bug while testing: the
  first version had the sign and accumulate bits swapped (MULL's own
  encoding came out identical to MULALU's), found by `mull_case17.s`
  and confirmed against goken directly. Added `mull_case17.s` (all 4
  variants computing the same product into 4 different register
  pairs, MULAL/MULALU accumulating onto pre-zeroed registers so all 4
  results should match, summed into the exit code). Two different
  multiplicands ($100000/$100001, not the same constant twice) to
  avoid the literal-pool-deduplication gap. PASS byte-identical +
  PASS same qemu-arm exit code.
- **Case 35** `mov PSR,R` (MRS) / **case 36** `mov R,PSR` (MSR) /
  **case 37** `mov $con,PSR` (MSR) -- real, reachable via the same
  "MOVW" mnemonic/gen mechanism as FP[CS]R (case 56/57): new `psrreg`
  (CPSR/SPSR, goken's D_PSR) and `PSRImsr of psrreg` on `mov_operand`.
  New `Codegen5.ml` helpers `gpsr_read` (MRS) and `gpsr_write_base`
  (MSR, shared by both the register and immediate source forms; case
  37's immediate goes through the existing `immrot` helper, erroring
  loudly like goken's own `diag()` if the constant isn't
  immrot-encodable rather than silently emitting wrong bytes -- no
  literal-pool fallback).
  Scoped limitation: the ".F" (flags-only write) suffix isn't wired
  -- at the time, xix's grammar had no dot-suffix-flag parsing
  mechanism at all (goken unifies .S/.P/.W/.U/.F/etc into one generic
  `scond` bitmask via a shared grammar rule; even the already-
  existing arith_cond/move_cond fields, e.g. Arith's own ".S", were
  hardcoded to `None`, never actually parsed from real .s text).
  Building that subsystem from scratch for one flag bit wasn't worth
  it on its own -- the default (unset, "full PSR write") is still a
  real, useful, independently-testable instruction shape. (That
  generic mechanism got built for real two cases later, for MOVM --
  see below -- but PSR's ".F" itself was never revisited since.)
  Added `psr_move_case35_36_37.s` (CPSR round-trip: read, write from
  register, write from immediate, read again). PASS byte-identical
  (334 bytes both sides) + PASS same qemu-arm exit code. SPSR verified
  ad hoc (not its own fixture, same reasoning as
  `fcr_move_case56_57.s`'s FPCR aside below): byte-identical against
  goken and traps identically as "Illegal instruction" on both sides
  (no SPSR access in user mode).
- **Case 38** `movm $con,oreg -> stm` / **case 39** `movm oreg,$con ->
  ldm` (ARM's block data transfer, multi-register load/store) --
  real, reachable syntax: `"MOVM [reglist],oreg"` (store) / `"MOVM
  oreg,[reglist]"` (load), a.y's `reglist` rule (register range or
  comma list, confirmed against goken directly that combining both in
  one list, e.g. `"[R4-R6,R14]"`, is a syntax error on BOTH sides --
  not an xix limitation). New `RegList of int` mov_operand (a plain
  bitmask) and `MOVM of movm_addr_mode * mov_operand * mov_operand`
  instr, direction inferred from which side is RegList vs Indirect
  (same convention as MOVE's src/dst).
  Unlike PSR's lone ".F" bit, MOVM's P/U/W address-mode suffix bits
  (".IA"/".DB"/".IAW"/".DBW"/etc) are essential to any real use (e.g.
  a function-prologue push is `"MOVM.DB.W [regs],(SP)"`) -- this is
  what finally justified building a real generic dot-suffix-flag
  grammar mechanism: `Parser_asm5.mly`'s `condf` rule and TSUF token,
  a left-recursive bitmask accumulator directly mirroring goken's own
  `cond: cond LS { $1 | $2 }` (a.y). Kept as a separate nonterminal
  from the existing `cond` rule (still used unchanged by every other
  instruction) so building this couldn't silently change what any
  already-tested production accepts -- only MOVM opts in.
  The S bit (goken's "load/store user-mode registers" / "restore CPSR
  from SPSR" special form) is real on goken but privileged-only --
  RFE already covers the one exception-return use that matters under
  this harness -- so it's rejected with a real grammar-time error
  rather than silently ignored. The F bit is also rejected, but for a
  completely different reason: goken's own C_UBIT and C_FBIT are
  literally the same physical bit (`1<<7`, `GO/C/cmd/5l/5.out.h`)
  reused for two unrelated meanings ("up" on MOVM, "flags-only" on
  MSR) -- confirmed directly that assembling `"MOVM.F.DB.W [...]"`
  with goken produces byte-identical output to `"MOVM.U.DB.W"`, a
  bit-packing accident rather than a real language feature nobody's
  .s text relies on. xix keeps ".F"/".U" as distinct tokens rather
  than replicating that aliasing, so MOVM.F is simply rejected
  instead of silently behaving like MOVM.U.
  Also needed 2 new shared punctuation tokens, TLBRACKET/TRBRACKET
  ("["/"]"), added to `Token_asm.ml`/`Lexer_asm.mll` like any other
  punctuation (RISC-V/other arches' `Parse_asmX.ml` just pass them
  through unused).
  Added `movm_case38_39.s`: two self-balanced push/pop pairs against
  the real stack pointer (R13, valid at Linux process entry) using
  ".DB.W"/".IA.W", one with a comma reglist and one with a range
  reglist, each restoring and summing its registers into the exit
  code. PASS byte-identical (398 bytes both sides) + PASS same
  qemu-arm exit code (66) -- a genuine functional round-trip.
- **Case 50** floating point store / **case 51** floating point load
  / **case 52** floating point store, long offset UGLY / **case 53**
  floating point load, long offset UGLY -- MOVF/MOVD to/from a plain
  register+offset address, the float analogue of case 20/21/30/31
  (int store/load). Unlike case 59-63/64-69 (see below), this one IS
  real, reachable .s syntax (optab.c's C_FEXT/C_FAUTO/C_FOREG/
  C_LEXT/C_LAUTO/C_LOREG rows, not C_ADDR's DLM-only or C_SHIFT's
  peephole-only path), so it's byte-tested against 5a/5l like every
  ordinary case.
  Required new AST, kept ARM-local rather than extending the shared
  `Ast_asm.move_size` (which has no Float case and is used by every
  arch): added `FImsr of freg` to `mov_operand` (the register side;
  the memory side still reuses Indirect/Entity and
  `base_and_offset_of_indirect` as-is, since float load/store still
  addresses memory through a plain *integer* base register) and a
  new top-level `MOVEF of A.floatp_precision * mov_operand *
  mov_operand` instr constructor (mirroring MOVE's shape but keyed on
  `floatp_precision` instead of `move_size`). `Object_file.version`
  bumped 17->18. Grammar: new TMOVF token, "MOVF"/"MOVD" keywords, and
  `gen` (already producing mov_operand) grew a bare `freg`
  alternative.
  New encoding helpers `gfsr`/`gfsr_vfp` (`ofsr()`/`ovfpmem()` in
  codegen.c), dispatched via the same `!Flags.vfp` convention as
  ArithF/CmpF/MOVWF/MOVFW. New `fimmoffset` (the short-offset range:
  a signed, word-aligned offset with magnitude < 1024, i.e. `gfsr`'s
  8-bit word-count field) alongside the existing `immoffset`. The
  long-offset case (52/53) needs an explicit `ADD REGTMP,Rbase,
  REGTMP` after loading the offset from the literal pool, unlike int
  case 30/31's gmem-register-offset shortcut -- FPA/VFP load/store
  has no register-offset addressing mode at all, only a fixed-size
  immediate, so the full address must be materialized in REGTMP
  first.
  Added `float_mem_case50_51.s`/`float_mem_vfp_case50_51.s`
  (short-offset store+load round-trip through MOVWF/MOVFW into the
  exit code) and `float_mem_longoff_case52_53.s`/
  `float_mem_vfp_longoff_case52_53.s` (long-offset, mirroring
  `longoff_case30_31.s`'s own two-different-offsets workaround for the
  literal-pool-deduplication gap). PASS byte-identical + PASS same
  qemu-arm exit code, all four (FPA/VFP x short/long-offset).
- **Case 56** move to FP[CS]R / **case 57** move from FP[CS]R --
  FPSR/FPCR moves. Unlike case 50-53, goken dispatches these through
  the exact same "MOVW" mnemonic/gen mechanism as an ordinary int move
  (a.y's `gen: ... | LFCR`), not a distinct mnemonic -- so this is a
  new mov_operand alternative (`FCRImsr of fcrreg`, `fcrreg = FPSR |
  FPCR`) handled inside the existing MOVE(Word,...) match arms (case
  20's store and case 21's load), not a new instr constructor. New
  `gfcr` helper (goken's fixed coprocessor-1 register-transfer
  encoding; the L bit, read vs write, added by the caller like
  `gfsr`/`gfsr_vfp`). No VFP variant exists for this in goken.
  Added `fcr_move_case56_57.s` (FPSR write+read round-trip into the
  exit code -- FPSR is genuinely accessible from user mode under
  qemu-arm). FPCR (same encoding, `fcr_val=2`) verified via an ad-hoc,
  non-committed test instead: byte-identical against goken, and traps
  as "Illegal instruction" identically on both sides (not emulated by
  qemu-arm). PASS byte-identical + PASS same qemu-arm exit code.
- **Case 59** `movw/bu R<<I(R),R -> ldr indexed` / **case 60** `movb
  R(R),R -> ldrsb indexed` / **case 61** `movw/b/bu R,R<<[IR](R) -> str
  indexed` -- **investigated, deliberately not implemented**: same
  story as case 62/63 below: goken's 5a grammar has no rule combining
  a shift operand with a base register, so this can't be spelled in
  real .s text; `compilers/5c/peep.c` confirms it's a compiler
  peephole (fusing a separate shift + load/store into one D_SHIFT-
  operand instruction) rather than something 5a ever parses. No way
  to build a byte-identical fixture against 5a/5l. Documented in
  `Codegen5.ml` instead of implementing something untestable. Also
  unlikely to ever be needed: xix's own future C compiler (occ) is
  planned to use c-- as a backend, which does its own peepholing and
  emits plain, unfused assembly, not this exact shape.
- **Case 62** `case R -> movw R<<2(PC),PC` / **case 63** `bcase` --
  **investigated, deliberately not implemented**: switch-statement
  jump table. Confirmed real (unlike MIPS's case 45/46, see
  `mips_port.md`): goken's 5c *does* emit ACASE/ABCASE for a
  dense-enough switch (`compilers/5c/swt.c`'s `swit2`). But goken's 5a
  (this port's actual, permanent target) has no grammar rule for
  either mnemonic -- only the separate, explicitly out-of-scope
  5ak/5lk lineage can parse "CASE"/"BCASE" as text at all -- so there
  is no way to ever build a byte-identical fixture against 5a/5l for
  this case.
  A prototype was built and worked (encoding confirmed bit-for-bit
  against `linkers/5l/codegen.c` and cross-checked once against
  goken's 5ak/5lk output; a hand-run fixture dispatched correctly
  under qemu-arm with xix's own o5a/o5l) by adding `Case`/`BCase`
  *generically* to `Ast_asm.ml`'s shared `virtual_instr`, parsed via a
  shared CASE/BCASE keyword duplicated in every arch's grammar, with
  only `Rewrite5.ml`/`Codegen5.ml` doing a real lowering. Reverted
  rather than kept: it required loosening two previously-clean,
  shared invariants that held across every arch --
  `assembler/Resolve_labels.ml`'s label-resolution pass never looking
  inside `Virtual` nodes at all, and `Rewrite{5,v,i}.ml`'s step1
  blanket "branch should not be set on virtual instr" check (BCase is
  the first Virtual construct ever reachable from real .s source
  carrying an unresolved branch target, which is what exposed both).
  Not worth that cost for a feature with no current consumer (nothing
  in xix emits switch-statement code yet) and no possible differential
  test against the actual target. Revisit only if a real driver shows
  up (e.g. a future xix C compiler backend wanting jump-table
  codegen) -- at that point the actual requirements would be concrete
  instead of guessed at, and might justify the shared-invariant
  changes on their own merits.
- **Case 64** `mov/movb/movbu R,addr` / **case 65** `mov/movbu
  addr,R` / **case 66** `movh/movhu/movb addr,R` / **case 67**
  `movh/movhu R,addr -> sb, sb` / **case 68** floating point store ->
  ADDR / **case 69** floating point load <- ADDR -- **investigated,
  deliberately not implemented**: all six key off optab.c's C_ADDR
  operand class. Confirmed in `span.c`'s `aclass()`: C_ADDR is only
  ever returned `if(dlm)`, both for the D_OREG-external-symbol path
  and the D_ADDR/SDATA path. `dlm` (`linkers/5l/globals.c`) is a
  single global set only by 5l's own `-u` flag ("produce dynamically
  loadable module", `main.c`) -- i.e. this whole operand class only
  exists for PIC/shared-library linking. This harness always links
  statically (never passes `-u`), so C_ADDR can never be produced,
  making cases 64-69 dead code under this harness's permanent scope,
  not just "not yet ported" -- same category as case 59-61/62-63
  (real codegen.c cases with no reachable path to a byte-identical
  fixture), just gated by a linker flag instead of a missing
  assembler grammar rule.
- **MCR/MRC** (coprocessor register move) -- real, reachable syntax:
  `"MCR con,expr,reg,creg,creg[,expr]"` (a.y's own comment: "MCR
  MRC"). Unlike every other ARM case, goken computes the final 32-bit
  word directly in the grammar action itself, no codegen.c dispatch
  at all, and emits it as a plain WORD pseudo-op -- so this reuses
  `Ast_asm.WORD` directly (`Parser_asm5.mly`'s `pseudo_instr`
  nonterminal) rather than adding a new instr constructor: no
  `Ast_asm5.instr`/`Meta_ast_asm5.ml`/`Codegen5.ml`/
  `Object_file.version` change needed at all, just grammar + a new
  `int_of_condition` helper (`Ast_asm5.ml`) to get the raw condition
  nibble without depending on `Codegen5.ml`'s Bits.t-returning
  `gcond` (assembler and linker are separate dune libraries). New
  `creg`/`oexpr` nonterminals; creg only supports the plain "C3"
  numeric spelling, not goken's rarer computed "C(expr)" alternate
  form.
  Added `mcr_mrc.s`: `"MRC 15,0,R0,C0,C0,0"` (read CP15's Main ID
  Register). PASS byte-identical (318 bytes) + PASS same qemu-arm
  exit code (112, MIDR's low byte) -- unlike FP[CS]R/PSR's privileged
  aside cases, qemu-arm's user-mode emulation actually implements this
  specific read rather than trapping, so it's a genuine functional
  round-trip. MCR (write side, `"MCR 15,0,R0,C1,C0,0"` writing SCTLR)
  verified ad hoc, same pattern as those asides: byte-identical
  against goken, traps identically as "Illegal instruction" on both
  sides.

## Open issues

- **Literal-pool value deduplication**: goken's `addpool()` reuses an
  existing pool word when a new entry's constant/address value
  exactly matches one already pending, instead of adding a duplicate
  -- NOT ported. Discovered while verifying case 30/31: a fixture
  using the same large offset twice diverges in pool *layout* from
  goken even though the actual load/store instructions it generates
  are correct. Low urgency (no test currently needs it -- every
  fixture that could trigger it is deliberately written with two
  *different* offsets/constants instead, e.g.
  `longoff_case30_31.s`/`float_mem_longoff_case52_53.s`/
  `mull_case17.s`), but real; will resurface for any fixture that
  repeats a big constant/address.
