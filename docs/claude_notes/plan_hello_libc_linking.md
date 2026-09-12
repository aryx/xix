# Plan: testing o5a/o5l against a real, complete C program

## Goal

Go beyond the hand-authored `.s` fixtures used throughout the
ARM/MIPS/ARM64/RISC-V differential-testing effort (see
`{arm,mips,arm64,riscv}_port.md`) by testing `o5a`/`o5l` against a
*real*, non-trivial C program: goken's `tests/c/hello_libc/hello.c`,
which links a real, reusable `lib_core/libc/libc.a` (unlike the
simpler `tests/c/mini*` tests' hand-rolled per-test stubs).

The original idea (the user's own): use goken's real `5c -S` to
compile `hello.c` plus its full transitive libc dependency closure to
Plan9 assembly text, concatenate it into one `hello_libc.s`, and see
if xix's own `o5a`/`o5l` can assemble+link it.

**Revised framing after this session's investigation** (the user's own
correction): the actual goal is *input-grammar parity* with `5a` --
whatever real, valid assembly `5a` accepts, `o5a` should accept too.
The intermediate `.5`/`.o5` object files never need to be equal (xix's
own object format is an OCaml `Marshal` format by design, unrelated to
goken's; only the final linked executable is ever compared -- see
`arm_port.md`). `5c -S`'s output turned out not to be a valid way to
probe that parity (see below), so it's no longer the mechanism, but
the parity goal itself stands.

## Key finding: `5c -S` output is not reassemblable, even by goken's own `5a`

`5c -S <file.c>` prints a human-readable, disassembly-style dump to
**stdout** (not `-o`, which still writes a real, separate, binary
object file). That dump uses a padding convention where every
instruction's print format always shows all of its operand slots,
comma-separated, even when unused:

```
BL	,print+0(SB)
CMP	$0,R6,
RET	,
END	,
```

This is **not valid input to `5a` itself**. Confirmed empirically: the
full `hello_libc.s` produced this way, fed back into goken's own real
`5a`, fails with the same shape of syntax errors `o5a` initially had
(`hello.s:12 syntax error, last name: BL ... saw ,`). There is no
"recompile the `-S` dump" pathway in this toolchain at all --
`-S` is a print/debug format, not source, regardless of which
assembler consumes it.

This was verified layer by layer during the session:
- `5c -S -o file.s foo.c > out.s` (redirecting stdout, since `-S`
  prints to stdout and `-o` writes a *separate* object file) does
  produce genuine-looking Plan9 ARM assembly text.
- Compiling `hello.c` alone this way, plus the ~140 `lib_core/libc`
  source files `mk -n -a 'objtype=arm' 'cputype=arm' 'GOOS=linux'
  install`'s dry-run reports as needed for a full `libc.a`, and
  assembling each individually with `o5a` (kept as separate object
  files -- see "Why not one concatenated file" below) surfaced real
  gaps one at a time as they got fixed:
  - `foo<>` static/local symbols (see next section) -- unblocked ~30
    files.
  - `BL\t,target`, `B\t,target`, `Bxx\t,target`, `CMP\t$0,R6,`,
    `RET\t,`, `END\t,` -- the padding-comma shapes. Initially "fixed"
    by adding lenient grammar alternatives, which unblocked most of
    the remaining files -- but see below, this was a mistake.
  - `RET.MI` (conditional return, real 5c output for predicated
    early-returns like `if(x<0) return -x;`) -- genuinely unhandled by
    `o5a` (RET had no condition support at all before this session).
  - `arch/arm/div.s`'s `Q = 0`-style `NAME = value` constant
    definitions -- a real, pre-existing, documented gap
    (`Parser_asm.ml`'s own header comment already lists "no support
    for 'NAME=expr;' constant definition" as a known limitation).
  - `CASE`/`BCASE` jump-table dispatch (switch statements in
    `fmt/dofmt.c`, `port/strtol.c`, `port/vlrt.c`, etc.) -- never
    reached a real conclusion, see "Open issues" below.

- **The pivotal check**: once `hello.s` alone (no libc) was fully
  "fixed" to assemble with `o5a`, the same file was fed to goken's own
  `5a` as a sanity check before going further -- and it failed
  identically. That's when the padding-comma leniency was identified
  as a mistake: `o5a` had been made into a *superset* of what `5a`
  accepts, which breaks the whole point of differential testing
  (silently accepting something the reference tool rejects can mask a
  real difference). All of that leniency was reverted.

## Why not one concatenated `hello_libc.s`

Plan9's `foo<>` symbols are scoped *per object file* (the linker
assigns each loaded object its own private id -- see
`linker/Types.ml`'s `Private of int` and `linker/Load.ml`). Every C
file that uses local string/float constant pools names them
identically (`.string<>+0(SB)`, etc.). Concatenating many originally-
separate `.c` files' `-S` output into one text file and assembling it
as a *single* `o5a` invocation collapses them all into one object,
so their `<>` locals collide. The correct architecture -- and the one
actually used while testing this -- is: compile+assemble each source
file separately (own `o5a` invocation, own object, own private-symbol
namespace), then link the whole set together with one `o5l` call
(`o5l` already accepts a list of input object files, see
`linker/CLI.ml`'s `link caps arch config xs chan`). This is also
closer to how goken itself actually builds a real program.

## What was real and worth keeping (found chasing this, independently validated against goken's real `5a`, not just the `-S` artifact)

Committed this session (`assembler: support Plan9's "foo<>"
static/local-symbol syntax; arm: conditional RET`):

- **`foo<>` static/local symbols**, across all 4 architectures. The
  shared `Lexer_asm.mll` never tokenized `<`/`>` at all (unrecognized
  character), even though every arch's `Parser_asmX.mly` already had a
  full `TIDENT TLT TGT offset TOPAR TSB TCPAR` grammar rule for it, and
  the linker's `Private`/`Load.ml` machinery already fully supported
  the resulting AST -- purely a missing lexer rule, not a missing
  feature. This affects **real, hand-written** `.s` too, not just
  compiler output -- goken's own `arch/arm/div.s` uses `save<>`,
  `rest<>`, `div<>` and failed to assemble with `o5a` before this fix.
  Confirmed goken's real `5a` accepts `foo<>` (`div.s` assembles fine
  there). New fixture: `tests/linker/arm_diff/static_symbol.s`
  (byte-identical + functional match against goken).

- **ARM conditional `RET` (`RET.MI`, etc.)**. `RET` never carried a
  condition at all -- `Ast_asm.virtual_instr.RET` is a bare, shared-
  across-archs constructor with no room for an ARM-specific condition
  type. Confirmed `RET.MI` is real, valid `5a` syntax (goken's own
  compiler emits it routinely for predicated early-returns). Added
  `Ast_asm5.CRET` (RFE-style: a real `instr` variant carrying a real
  condition, expanded in `Rewrite5.ml`'s step2 the same way as plain
  `RET` once the enclosing `TEXT`'s frame size is known -- either
  `B(IndirectJump rLINK)` for a leaf procedure or a
  `MOVW.P autosize(SP),PC`-style restore for a framed one).
  - **Leaf-procedure case**: validated byte-identical against goken's
    real `5a`/`5l`. New fixture: `tests/linker/arm_diff/cret_leaf.s`.
    Required also fixing `Codegen5.ml`'s `B` instruction encoder,
    which unconditionally asserted `cond = AL` and hardcoded `gcond AL`
    in its `IndirectJump` byte-emission -- both were pre-existing
    artifacts of `B` never having carried a real condition before.
  - **Framed (non-leaf) case**: attempted the same
    `MOVW.P.cond autosize(SP),PC` expansion, but a differential test
    against goken's real `5a`/`5l` showed goken emits a shorter/
    different byte sequence -- genuinely not yet reverse-engineered.
    Guarded with `raise Todo` in `Rewrite5.ml` (same convention as
    `riscv_port.md`'s case-18 guard) rather than silently emitting
    wrong bytes. **This is the main concrete open item from this
    session** -- see "Open issues".

## Real, still-open parity gaps (confirmed against goken's real `5a`, not `-S` artifacts)

Checked directly against goken's real `5a` after the pivot away from
`-S`-round-tripping, per the user's "input-grammar parity" framing:

- **`NAME = value` top-level constant definitions**
  (`arch/arm/div.s`'s `Q = 0`, `N = 1`, etc.) -- confirmed real, valid
  `5a` syntax (goken's real `5a` assembles `div.s` cleanly end to end);
  `o5a` doesn't support this construct at all (lexer error:
  "unrecognized character: '='"). This is the one limitation
  `Parser_asm.ml`'s own header comment already flags as deliberately
  unimplemented ("no support for 'NAME=expr;' constant definition...
  but can use cpp #define for that") -- except goken's own real source
  uses the native form, not a workaround, so a real program using this
  file as-is needs it. Implementing it needs a name→value environment
  threaded through constant-expression evaluation at parse time
  (`Parser_asm.ml`'s own comment explains why this was deliberately
  avoided: "allows to evaluate constant at parsing time and avoid the
  need to build an expr AST" -- i.e. it's a real, if small, evaluator-
  order design decision, not just a grammar rule).

- **`CASE`/`BCASE`** (switch-statement jump-table dispatch, used in
  `fmt/dofmt.c`, `port/strtol.c`, `port/strtoul.c`, `port/strtoll.c`,
  `port/strtoull.c`, `port/vlrt.c`, `fmt/fltfmt.c`, `fmt/strtod.c`,
  `os/linux/open.c`, `os/linux/notify.c` -- 10 of the 142 files in a
  full `libc.a` build for arm/linux). **Inconclusive**: a hand-written
  test fixture using the `-S` dump's *shape* (minus the padding
  commas) was rejected by goken's own real `5a` too (`syntax error,
  last name: .LS ... saw LCOND`), meaning the real syntax for `CASE`/
  `BCASE` hasn't actually been found yet -- neither confirmed as a
  real `o5a` gap nor ruled out. Needs either finding real, valid
  example usage in goken's own hand-written `.s` sources (a grep
  across the tree, not yet done) or reading goken's own `5a` grammar/
  lexer source for the real syntax, before concluding anything.

## Possible paths forward

1. **Keep testing `o5a` against real, hand-written `.s` files already
   in goken's tree** (not `-S` output, not files this project authors
   itself) -- e.g. every file under `lib_core/libc/arch/*/`,
   `lib_core/libc/syscall/os/*/`, and any other hand-written `.s` in
   goken's own history. This is the direct, low-risk way to keep
   probing genuine `5a`-input parity: for each such file, "does
   `o5a` accept what `5a` accepts" is a yes/no question answerable by
   running both, no fixture-authoring judgment calls involved. `div.s`
   (blocked on `NAME = value`) and the `CASE`/`BCASE` question above
   are exactly this kind of check, just not yet exhaustive.

2. **Resolve the framed-`CRET` gap** -- the one concrete, scoped,
   already-diagnosed open item from this session. Needs a real 5c-
   compiled (or hand-written, real-`5a`-verified) example of a
   conditional return inside a framed procedure, disassembled/
   byte-compared against goken's actual output, to find the real
   expansion shape (it's shorter than the naive "same as unconditional
   RET, but predicated" guess -- goken is doing something more
   compact, not yet identified).

3. **`NAME = value` constant definitions** -- implement the small
   name→value evaluation-time environment `Parser_asm.ml` deliberately
   avoided, scoped to just this feature (not a general redesign of the
   parser's "evaluate constants at parse time, no expr AST" approach).

4. **Once individual-file parity is solid, revisit the original
   "build a real, complete program" goal** -- not via `5c -S`, but via
   the separate-compilation-then-link architecture described above:
   hand-author (or otherwise obtain, in genuinely valid `5a` syntax)
   the small set of real libc `.s`/`.5`-equivalent sources
   `hello.c` actually needs, assemble each individually, link with one
   `o5l` call. Note even the *minimal* dependency closure for
   `print()`+`exit()` alone pulls in a large fraction of libc.a (the
   whole `fmt`/`dofmt`/`vfprint` formatting engine, which is exactly
   where the `CASE`/`BCASE` and `NAME = value` gaps live) -- so this
   is a substantial undertaking even once 1-3 are resolved, not a
   quick follow-up.

5. **Separately, if real object-file-level testing (not just
   assembly-source-level) is ever wanted**: write a translator from
   goken's own compiled `.5` object files into `o5a`'s own object
   format (an OCaml `Marshal` format by design, unrelated to goken's --
   see `arm_port.md`). This would let real `libc.a` binaries (not
   their reconstructed-from-`-S` source) feed `o5l` directly, sidestepping
   the whole "is this text re-assemblable" question entirely. A
   genuine, separate reverse-engineering task, not attempted this
   session.

## Status

No task is in progress. This is a paused investigation with one
committed, validated deliverable (`<>` symbols + leaf `CRET`, all 4
archs regression-tested clean) and a clear list of what's next,
above, for whenever this is picked back up.
