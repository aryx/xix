# The code map's configs for xix: the brief

Written 2026-09-30 by Claude for the agents that wrote xix's
`.codemapconfig` files, one area each; kept for the next pass.

## What to read first

- The guidelines, all of it:
  `~/github/ocaml-elm-playground/docs/claude_notes/codemapconfig_guidelines.md`
  (the sections on ~/ix and ~/principia matter most: xix is OCaml like
  ~/ix, and ports ~/principia's C programs).
- The format: `~/github/ocaml-elm-playground/docs/manual/codemap.md`,
  section 12.
- xix's root config `~/github/xix/.codemapconfig` and its shapes
  `~/github/xix/skeletons.libsonnet` (`chain`, `cli`, `module`, `calls`,
  `parts`): read them, do not edit them. Use the shapes (`local skeletons
  = import '../skeletons.libsonnet';`, with as many `../` as needed).
- xix's `CLAUDE.md` (the capabilities, the Main/CLI convention).
- A worked example of the style: `~/principia/.codemapconfig` and any
  config under `~/principia/builders/mk/` or `~/principia/shells/rc/`
  (the C originals of omk and orc).

## The project explains itself

- Each port has a literate book in its `docs/` (`builder/docs/omk.nw`,
  `shell/docs/orc.nw`, `compiler/docs/occ.nw`, `assembler/docs/oas.nw`,
  `linker/docs/olk.nw`, `vcs/docs/ogit.nw` and `Overview.nw`,
  `windows/docs/orio.nw`, `editor/docs/oed.nw`,
  `generators/docs/CompilerGenerator.nw`): their `\section{Code
  organization}` (a table: a file, its role, its entities) and
  `\section{Software architecture}` (the pipeline) are the skeletons and
  summaries already judged by the author. Copy their words and chains
  before inventing any.
- `docs/index.html`: the programs, what they port (omk is mk, orc rc,
  occ 5c, oas 5a, olk 5l, oed ed, ogit git, orio rio).
- The C originals are in `~/principia` (a program's `.c` there, its
  config written yesterday): a summary may say what the port changed
  ("the AST built first, evaluated after: 5c did both at once").
- The files' header comments; `(* See X.mli *)` means the header is in
  the `.mli`.

## The rules

- **Create only new `.codemapconfig` files, in your area only.** Edit no
  source, no other config, not the root's, not `skeletons.libsonnet`,
  not `.codemapignore`. If the root or the shapes need a change, say it
  in your report.
- **Every directory holding a source (`.ml`, `.mli`, `.c`, `.h`, `.s`)
  needs its own config**: a parent's `files:` cannot describe a file
  below it (a mistake), and a folder's skeleton is read only from its
  own config. A directory without sources but with subdirectories gets a
  config with its `summary`. Directories named `TODO/` and the paths in
  `.codemapignore` are off the map (`tests/*/`: the tests' inputs).
- Each config: `generated: { by: 'claude-opus-5-5', on: '2026-09-30' }`,
  its `summary`, `files:` (each with `summary`, `digest`, and where they
  earn it `capitals`, `important`, `links`), its `skeletons`, maybe a
  `tours` for a program's directory.
- Items: `at`, `say`, `weight` (3 for the two or three that matter most,
  1 otherwise); a bone's `role`; a joint's `from`, `to`, `say` whose ends
  must be bones of its own skeleton (one bad joint makes the whole
  config unread: every file under it "missing").
- Skeletons: every folder of two sources or more, and every module of
  150 lines or more, needs its own (bones mostly in it). A program
  (`Main.ml` calling `Cap.main`, or a `let () =`/`let _ =` reading argv):
  `skeletons.cli('name', [[anchor, role, say], ...])` from its
  directory, the core after CLI.main, extended with `bones+:`/`joints+:`
  for a loop. A whole-file bone (a path, no anchor) counts toward its
  file.
- Capitals: the hubs first. -check names them ("a hub ... with no
  capital"): its main types and functions. At most three a file; most
  files have none. `Cap.mli`, `Ast_asm.ml`, the ASTs, `Common`, `Fpath`,
  `Chan`, the kernel's `u.h` and `mlvalues.h` are what the whole project
  is written with: describe them first and best.
- Anchors: `def:` and `type:` first; `comment:"words"` (words on one line
  of a comment), `code:"words"` (the first line of code holding them),
  `section:`; never `line:` unless nothing else reaches. OCaml-light
  forbids functors and optional arguments: the code is plain, anchors
  land easily. In the kernel's C, Plan 9's static prototypes make names
  "defined twice": check the line the facts give; `code:"name(Type arg"`
  finds the body.
- Summaries: what it is for, one sentence of 60 to 100 characters, no
  "This file". Say what it ports ("mk's graph of targets, from graph.c")
  and what is new.
- Notes (`say`): 30 to 60 characters, why, not what.
- Name the real code's findings in important lines (a bug, a stale
  comment, dead code: `kernel/Old/`, `concurrency_`, the `todo/`
  directories -- say what they are).

## How to work

- Your scratch directory is your own (given in your prompt): the facts
  and check outputs go there, never in the repository.
- The tools, from `~/github/ocaml-elm-playground` (do not build it; it
  is built):
  - `./bin/tinybox codemap -facts ~/github/xix <dir>` prints a directory's
    brief (its files, digests, definitions and uses). One directory at a
    time; each run reads the whole project (a second or two).
  - `./bin/tinybox codemap -check ~/github/xix` checks every config: 0
    mistakes and 0 missing in your area is done (grep your paths).
    -check reports one structural mistake per config at a time: fix and
    rerun until two runs agree.
- Other agents write the other areas at the same time: the check shows
  their mistakes and missing too; ignore them.
- Jsonnet: an apostrophe ends a single-quoted string (use double
  quotes); named arguments are `name=value`.

## Report

At the end, report: the configs written (a count and the list of
directories), what -check says for your area, and your LESSONS: what
this brief, the guidelines, the checks or the facts got wrong for this
codebase (a tool's mistake, a missing shape), shortest first.
