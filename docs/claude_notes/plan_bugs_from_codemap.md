# Bugs found while writing the code map's configs

Written 2026-09-30 by Claude. Seven agents described every directory of
xix for tinybox's code map (`.codemapconfig`, see
`codemap_brief_xix.md`). They read each file closely, and on the way
found real bugs, dead code and stale comments. Each finding is also an
important line in its directory's config, so the map shows it where it
is. This file gathers them into a plan.

**Status column**:
- *checked*: Claude looked at the line in the source after the pass.
- *reported*: an agent's reading; not re-checked, and nothing was run.

Nothing here has been fixed yet. The line numbers are from 2026-09-30.

## 1. Bugs likely to bite (fix first)

| # | Where | What | Status | Fix |
|---|---|---|---|---|
| 1 | `vcs/repository.ml:476`, `vcs/changes.ml:68,156` | Commit c588b361 ("use Fpath.t everywhere") replaced the walks' starting path `""` with `Fpath.v "XXX"`. The paths built from it start with `XXX/`, so checkout, reset, clone and pull (all through `set_worktree_and_index_to_tree`) would write under `XXX/`; status, show and log would print `XXX/` paths. | checked (code); not run | A root path that `Fpath.(/)` leaves out, or relative paths built without a prefix. Add a test: clone then status. |
| 2 | `vcs/diff3.ml:95` | `if lo < leno \|\| la < lenb \|\| lb < lenb`: the second test should be `la < lena`. | checked | `la < lena`. |
| 3 | `vcs/cmd_reset.ml:16` | `String.sub commit.Commit.message 0 40` raises on any message shorter than 40 characters. | checked | `min 40 (String.length ...)`. |
| 4 | `kernel/scheduler/Scheduler.ml:90` | `for i = Scheduler_.nb_priorities -1 to 0 do` never runs, so `find_proc` never finds a process. | checked | `downto`. |
| 5 | `kernel/concurrency/Spinlock.ml:34`, `Ilock.ml:33` | The inner loop spins `while not !(x.hold)`: it waits for the lock to be *taken*, backwards. Hidden today because `Tas.tas` always succeeds. | checked | `while !(x.hold) do`. |
| 6 | `kernel/time/Time.ml:29` | `ms_to_ns ms = ms * 100000`: a millisecond is 1,000,000 ns. | checked | `* 1_000_000`, written `* 1000000` for ocaml-light. |
| 7 | `kernel/core/Arch.ml:45` | `kzero = 0x40000000` while `mem.h` says `KZERO` is 0x80000000. | checked (Arch.ml); mem.h reported | Decide which one is right, then make the other match. |
| 8 | `caps/src/caps/CapUnix.ml:39` | `unlink caps file` checks `file` with `caps#open_out`, then returns `Unix.unlink` unapplied, so its type is `string -> string -> unit` and the path actually unlinked is never checked: a hole in the capabilities. | checked | `Unix.unlink file`. |
| 9 | `lib_parsing/Parsing_.ml:304` | `if i < 1 && i >= env.current_rule_len`: never true, so the bounds check never fires. `Parsing_.debug` also defaults to true (reported). | checked | `i < 1 \|\| i > env.current_rule_len` (check the upper bound's off-by-one against the callers). |
| 10 | `lib_core/commons/Console.ml:170-177` | `sprintf` fails with "TODO: style not handled" on `Bold`, `Underlined` and the others whenever highlighting is on, and it is on by default. `Console.bold` would always raise; nothing calls it yet. | checked | Emit the ANSI codes for them. |
| 11 | `kernel/Libmemdraw/alphadraw.c` `writebyte` | Always writes 4 bytes, so it runs past the end of images shallower than 32 bits. Principia has since fixed it; the xix copy has not. | reported | Port principia's fix. |
| 12 | `editor/Env.ml:30` | `tfname = Fpath.v "/tmp/oed.scratch"`: one fixed scratch file, shared by every oed running at once. | checked | A temporary file per process (via a `Cap.tmp`). |
| 13 | `editor/Commands.ml` `substitute` | Never sets `fchange`, so `q` does not warn after an `s`. | reported | `e.fchange <- true` when something was substituted. |

## 2. Unfinished, and failing when reached

- **orc** (`shell/Interpreter.ml`): fails with "TODO" on `for`, `fn`,
  backquotes, `&`, most redirections, concatenation and index. `Glob`
  only logs; `Glob.ml` and `Heredoc.ml` are empty. Functions are not yet
  called by `op_Simple`. `-p` is accepted and ignored.
- **ogit**:
  - `ogit show <arg>` always fails: `parse_objectish` raises Todo.
  - Commits get a hardcoded timezone of -7.
  - `branch -d` deletes branches that are not merged.
  - Pull and push only fast-forward; the git:// client is a stub.
  - `set_ref_if_same_old` never compares the old value (the check is
    commented out), and `with_file_out_with_lock` takes no lock.
- **The kernel**:
  - `Page.alloc` needs more than 100 free pages, and `_init_allocator`
    is Todo: every allocation fails.
  - `Hooks.Scheduler` is never set, so a contended qlock fails.
    `Hooks.Chan.close`'s error message says `chan_of_filename`.
  - `exec` stops at `parse_header` (Todo).
  - C side: `fault` panics ("TODO: implement fault in OCaml"),
    `arch__syscall` panics, and `addclock0link` panics.
  - Under dune, the `thread_sleep`/`thread_wakeup` stubs in Scheduler,
    Rendez and Test fail, because the system's Thread lacks them (the
    1997 bytecode threads in `threads/thread.ml` have them).
- **orio**:
  - A resize message (`'r'`) from `/dev/mouse` kills the mouse thread
    (`failwith`).
  - `Wm.new_win` forks while threads are running; the critical section
    is a TODO.
  - The 9P server has no readdir.
  - `Threads_window.error` silently drops errors.
  - `Baselayer.alloc` returns an id even after 25 failed tries.
- **oyacc**:
  - SLR only; `Lalr.ml` is empty.
  - Conflicts are never reported: the action table is a list, so a
    conflict is just two entries.
  - `Check.check` is called by no one.
  - The LR(0) automaton is dumped on every run.
- **omk**:
  - `$MKSHELL` defaults to sh, not rc.
  - `Graph.apply_rules` has no guard against rules applied forever
    (mk's `$NREP`).
  - `<|cmd`'s temporary file is never deleted.
- **The toolchain**:
  - `Location_cpp.final_loc_of_loc` has "bugfix: wrong!! TODO" on the
    Eof case, so a line reported after an `#include` ends may be wrong.
  - `Library_file.is_lib_filename` knows `.oa`, `.oa5`, `.oav` and
    `.oai`, but no suffix for 7, 6 or j.
  - `A_out` handles ARM only (MIPS is a TODO).
  - `Datagen` says "TODO: what about 64 bits arch?" on address DATA.
  - `compiler/Rewrite.ml` is nearly the identity: pointer arithmetic
    and casts are "todo mandatory".
  - `Codegen.codegen` returns `locs = []`.

## 3. The capability rule broken

These bypass `Cap` (the semgrep rules should catch some of them: check
why they don't).

- olex and oyacc: no `Cap.main`; they read `Sys.argv` and call `open_in`
  directly.
- `utilities/files/wc.ml` reads `stdin` directly (cat.ml goes through
  `Console.stdin caps`); `pwd.ml` calls `Sys.getcwd`.
- `debugger/ksym.ml`: `Arg.parse` and `exit`, no `Cap.main`.
- omk: `CLI.build_target` prints "already up to date" with
  `print_string`; `Shell.exec_shell` ends with Stdlib `exit`.
- orc reads stdin with `Lexing.from_channel stdin`, not `Cap.stdin`.
- `Cmd.run` ignores its `_caps` and calls `Sys.command` (nothing in xix
  uses Cmd).
- `windows/tests/hellorio.ml` calls `exit`, not `CapStdlib.exit`.
- `CapRandom` is missing from the caps mkfile's SRC, and its wrappers
  never touch `caps#random`.

## 4. Debugging left on, and other leftovers

- `kernel/Bcm/trap.c:29`: `Debug = 1` prints "irq()" on every
  interrupt (checked). `clock.c` prints on every tick.
- `kernel/Port/portclock.c` sets the tick 3 times slower, with a TODO
  to put it back. `tod.c`'s `todfix` link is turned off.
- `kernel/Port/print.c`'s `_efgfmt`: `%e %f %g` print nothing in the
  kernel. `vfprint` and `fprint` end in `kernel/fakes.c`'s fake `write`,
  and `getconf` always returns nil, so the "vgasize" parsing in
  `screen.c` never runs.
- `vcs/diff_basic.ml` is compiled but called by nobody.
- `vcs/index.ml`'s `write_mode` uses `_` digit separators, which
  ocaml-light misreads (CLAUDE.md's rule).
- Tests that no longer compile against the capability API:
  `windows/tests/test_rio_graph_app1.ml` and
  `lib_graphics/input/tests/hellodraw2.ml` call `Draw.init`,
  `Keyboard.init` and `Mouse.init` without capabilities.
- `lib_gui/lib_graphics/draw/draw_graphics.ml`: `conv_point` does not
  flip y, despite its comment; every function there raises Todo.
- `Mouse_action`'s `SweepRightClicked` tests the left button (a QEMU
  workaround): say so in a comment, or make it configurable.
- `utilities/misc/tree.ml` is not valid OCaml (prose after the code, an
  optional argument), and nothing builds it. `scripts/hello_script.ml`
  needs semgrep's libraries and has the typo `[@defaul false]`.
- Not built at all: `lib_core/commons/Ftype.ml`,
  `lib_core/concurrency/todo/`, `vcs/todo/` (18 empty placeholders),
  the macroprocessor's CLI.ml, CLI.mli and Main.ml (its dune executable
  is commented out), `linker/tools/size.ml` and `strip.ml`,
  `Optimize5.ml` and `Optimizev.ml`, the kernel's empty `sys*.ml` in
  files/, directories/, namespaces/ and ipc/, and `kernel/arch/`.

## 5. Build files and comments out of date

- `kernel/mkfile`: its list of OCaml modules is stale (`core/spinlock_.cmo`,
  `processes/proc.cmo`, `syscall.cmo`). It also names `libc/`, `bcm/`,
  `byterun/` in lowercase while the tree has `Libc/`, `Bcm/`, `Byterun/`:
  it builds only on a case-insensitive filesystem.
- `vcs/mkfile` still lists sha1.ml, zip.ml, unzip.ml and compression.ml,
  which moved to lib_core/, and links only commons; dune is what builds
  ogit.
- Headers:
  - `assembler/CLI.ml` still lists "port 7a, 6a" as todo.
  - `linker/CLI.ml` says "the Plan 9 ARM/MIPS linkers".
  - `compiler/tests/Test_compiler.ml` says "Regression tests for rc".
  - `Check_asm5.ml` points to `resolve_labels5.ml`.
  - `Exec_file`'s "TODO: Elf64" is done.
  - grep's `CLI.ml` header is hello's "Toy hello program", copied.
  - hello's `CLI.ml` defines `module Str = Re_str` and never uses it.
- `Cap.ml` puts the "mix of fs and process_multi" note on `fs`; it
  belongs to `exec`, as in the .mli. `CapUnix.mli` points to a
  `CapExec.ml` that does not exist.
- `lib_core/compression/zip.ml`'s deflate is not DEFLATE.
- `plan9support.h` guards with `OS_PLAN9` where every other stub uses
  `OS_PLAN9_APE`.
- `keyboard.h` in the kernel is included by no C file.
- `shell/docs/orc.nw`'s "Code organization" names `Op_REPL.ml`,
  `Interpret.ml` and `Trap.ml`; the files are `Op_repl.ml` and
  `Interpreter.ml`, and there is no `Trap.ml`. `vcs/docs/ogit.nw`'s
  table names files that moved to lib_core/ or were never written.

## Plan

1. Fix section 1, one commit each, with a test where the testo suite
   can reach it (diff3, cmd_reset, Parsing_, Console, CapUnix; the
   kernel's through `kernel/Test.ml`). Confirm #1 first by running ogit
   clone and status.
2. Look at section 3 with `make check`: a rule that should have caught
   one and didn't is worth fixing in `semgrep.jsonnet`.
3. Decide, per item of sections 2 and 4, whether to finish it, delete
   it, or keep it with a comment that says it is unfinished.
4. Section 5 whenever the file is next touched.
5. After each fix, rerun `tinybox codemap -check ~/github/xix`: the
   configs' digests will say which files changed, and the important
   line marking the bug should be removed from that directory's config.
