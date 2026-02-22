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

- Must stay compatible with OCaml ≥ 4.09.1 and with `ocaml-light` (minimal stdlib)
- External dependencies are intentionally minimal: only `stdcompat`, `ocamlfind`, `ppx_deriving`
- Warnings `-33` (unused open) and `-6` (label omission) are suppressed for ocaml-light compatibility
- `open Xix_*` module opens are stripped by `scripts/remove_xix_open.sh` for ocaml-light builds
- `dune-project` uses lang 2.7 (not 3.0) to avoid `.pp.ml` file issues with codegraph indexing
