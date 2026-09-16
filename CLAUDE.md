# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Xix is an OCaml port of Plan 9 programs. It is mostly not ready yet. The repo produces tools like `omk` (mk build tool), `orc` (rc shell), `o5a/o5l/o5c` (ARM5 assembler/linker/C compiler), `oed` (ed editor), `ogit` (git client), `ogrep`, and others.

## Build System

Xix has two parallel build systems:

- **dune** – used for editor support (merlin) and as the primary way to build during development
- **mk/omk** – the "real" build system (Plan 9 mk port), must always work independently of dune

### Commands

```sh
# Bootstrap omk and orc from scratch (only needed once, or after clean)
./bootstrap-mk.sh
# Adds bin/omk and bin/orc; then set up PATH:
source env.sh   # sets PATH=./bin:$PATH and MKSHELL

# Build main binaries via dune (default/most common)
make            # builds omk, orc, o5a, o5l, o5c, ova, ovl, ovc, oed, ogrep, etc.
make all        # builds everything with dune
dune build

# Build via mk (after bootstrapping)
./bin/mk

# Clean
make clean      # dune clean + mk clean
dune clean

# Run all tests
make test       # dune build + ./test

# Run a specific test by name pattern
dune build && _build/install/default/bin/test -s <pattern>
# e.g.: _build/install/default/bin/test -s hello

# Lint (requires osemgrep)
make check      # osemgrep --experimental --config semgrep.jsonnet --strict --error
```

## Architecture

### Directory Structure

| Directory | Purpose |
|-----------|---------|
| `lib_core/` | Core OCaml libraries (commons, regexps, parsing, printing, etc.) |
| `builder/` | `omk` – Plan 9 mk build tool port |
| `shell/` | `orc` – Plan 9 rc shell port |
| `assembler/` | `o5a` – ARM5 assembler (also `oi`, `ov` variants for x86/RISC-V) |
| `linker/` | `o5l` – linker |
| `compiler/` | `o5c` – C compiler front-end |
| `kernel/` | Plan 9 kernel port (only works on Plan 9, needs OCaml4 ThreadUnix) |
| `editor/` | `oed` – ed editor port |
| `vcs/` | `ogit` – git client |
| `generators/` | `olex`/`oyacc` – ocamllex/ocamlyacc ports |
| `macroprocessor/` | cpp-like macro processor |
| `tests/` | Test suite entry point (`Test.ml`) using the `testo` library |
| `lib_graphics/`, `windows/` | Plan 9 graphics / windowing (Plan 9 only) |

### Capabilities System (Critical Architectural Pattern)

All programs use an OCaml object-type-based capability system defined in `lib_core/commons/Cap.mli`. This is enforced by semgrep rules in `semgrep.jsonnet`.

**Rules:**
- Never call `Sys.argv` – use `CapSys.argv caps`
- Never call `exit` – use `CapStdlib.exit caps`
- Never call `open_in` / `open_out` – use `FS.with_open_in caps` / capability variants
- Never call `Sys.chdir`, `Unix.chdir`, `Unix.fork`, `Unix.system`, `Sys.getenv`, etc. – use the `Cap*` wrappers
- Never use `Obj.magic` (would allow forging capabilities)

Every program's `Main.ml` must call `Cap.main` as the sole entry point:

```ocaml
let _ =
  Cap.main (fun (caps : Cap.all_caps) ->
    let argv = CapSys.argv caps in
    Exit.exit caps (Exit.catch (fun () ->
        CLI.main caps argv))
  )
```

Functions declare needed capabilities in their type signature using OCaml row-polymorphic objects:
```ocaml
let foo (caps : < Cap.stdout; Cap.open_in; ..>) = ...
```

### Module Conventions

- Each subsystem has: `Ast.ml`, `Lexer.mll`, `Parser.mly`, `Parse.ml`, `CLI.ml`, `Main.ml`
- `Main.ml` is only the entry point (calls `Cap.main` then `CLI.main`)
- `CLI.ml` handles argument parsing and dispatches to the real logic

### OCaml Compatibility Constraints

- Must stay compatible with OCaml ≥ 4.09.1 and with `ocaml-light` (minimal stdlib) — see
  "ocaml-light Compatibility" below for the details
- External dependencies are intentionally minimal: only `stdcompat`, `ocamlfind`, `ppx_deriving`
- Warnings `-33` (unused open) and `-6` (label omission) are suppressed for ocaml-light compatibility
- `open Xix_*` module opens are stripped by `scripts/remove_xix_open.sh` for ocaml-light builds
- `dune-project` uses lang 2.7 (not 3.0) to avoid `.pp.ml` file issues with codegraph indexing

## ocaml-light Compatibility

`dune build` passing is **not** sufficient proof the mk/`omk` build works — ocaml-light's
`ocamlc` is a much older/stricter compiler than whatever dune uses, and only
`make build-docker-light` (or `omk depend && omk all` inside a `padator/ocaml-light` container)
actually exercises it. It isn't run routinely, so don't assume it's currently green — check
before relying on it.

ocaml-light has only **partial** support for labeled/optional arguments, and none for
functors; avoid all three in new code:

- Labeled arguments: only the old, fully-spelled-out `~label:pattern` form parses (at both
  definition and call sites) — no punning sugar (`~label`, `~(label : ty)`; `~label:label`
  is fine but `~label` alone is not). Labels are also purely positional: ocaml-light warns
  "use of label ~x: (skipping it)" and does not check the name, so a call site with
  swapped label order silently binds wrong.
- Optional arguments (`?label`) don't parse at all, in any form — use a plain, explicit
  `... option` parameter instead.
- Avoid `open`ing two modules that both define a same-named constructor/record field and
  relying on type-directed disambiguation to pick the right one from context (e.g. an outer
  type annotation, or `Module.field = ...` on just the first field of a record literal) —
  ocaml-light doesn't do that inference; it resolves a bare constructor/label to whichever
  open module defined it last (or "Unbound" if none currently open it), so qualify explicitly
  (`Module.Constructor`, every field of a record literal) whenever there's any ambiguity.
- `_` digit-group separators in numeric literals (`0x8000_0000`) don't lex — ocaml-light
  silently mis-tokenizes them into two separate tokens.
- `let rec x = {...; f = y; ...} and y = {...}` (building a record graph via mutual `let
  rec`) only parses when at least one side is a function; a record literal as a `let rec`
  RHS is always rejected ("not allowed as right-hand side of `let rec'"), even when the
  "cycle" isn't a real cycle (e.g. `x` merely references `y`, `y` never references `x`).
  Build such chains tail-first with plain sequential `let`s instead.
- Its `Int64`/`Int32` have no `bits_of_float`/`float_of_bits` (and `Printf` has no `%L`), and
  `List` has no `find_opt`/`rev_map`/`rev_append`. See `lib_core/commons/Bits_of_float.ml` for
  a pure-arithmetic (frexp/ldexp-based) replacement for the float one; the others need simple
  inline workarounds at their call sites. (`Hashtbl.find_opt` and `List.concat_map` used to be
  missing too, but got added upstream in `ocaml-light` — don't route around those two anymore.)
- Objects are supported (used throughout for the capability system, `Cap.*`) but avoid them
  for anything else — same for functors, which ocaml-light doesn't support at all.
- When working around one of these, leave the recent-OCaml original in a comment alongside
  the workaround (see any of the above files for the established comment style), so the
  "real" way isn't lost.
