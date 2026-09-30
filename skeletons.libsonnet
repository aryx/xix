// The skeletons xix's programs share, for tinybox's code map
// (~/github/ocaml-elm-playground: its docs/manual/codemap.md, and the
// guidelines, docs/claude_notes/codemapconfig_guidelines.md). A shape
// is a function of files, its bones' anchors given.
{
  // a chain: [anchor, role, say] each, the first calling (or handing
  // its data to) the second, ... -- a path across files (the books'
  // "Software architecture": mk's parse -> eval -> build_graph -> ...);
  // the first step's say is unused
  chain(name, steps):: {
    name: name,
    bones: [{ at: s[0], role: s[1] } for s in steps],
    joints: [{ from: steps[i][0], to: steps[i + 1][0], say: steps[i + 1][2] } for i in std.range(0, std.length(steps) - 2)],
  },

  // a program of xix: its Main.ml runs Cap.main (the capabilities, the
  // only entry), which calls CLI.main (the flags), which hands the
  // arguments to the core: a chain as above ([anchor, role, say] each,
  // the first called by CLI.main). Extend with bones+: and joints+: for
  // a loop (the shell's read, eval, again).
  cli(name, core, main='Main.ml', cli='CLI.ml:def:main')::
    self.chain(name, [
      [main, 'the program: Cap.main, the capabilities', ''],
      [cli, 'the command line: flags, arguments', 'the capabilities'],
    ] + core),

  // a one-file program (ar.ml, nm.ml, ogit's main.ml, the utilities):
  // its Cap.main and its main in [file], then its core as in cli
  onefile(name, file, core, main='def:main')::
    self.chain(name, [
      [file + ':code:"Cap.main"', 'the program: Cap.main, the capabilities', ''],
      [file + ':' + main, 'the command line: flags, arguments', 'the capabilities'],
    ] + core),

  // a module's star: [center] (a bare name) calls each of [callees]
  // ([name, role, say] each), all defined in [file] (build_graph
  // calling its checks)
  module_calls(file, name, center, center_role, callees)::
    self.calls(name, file + ':def:' + center, center_role,
               [[file + ':def:' + c[0], c[1], c[2]] for c in callees]),

  // a thread's loop over Event.select (orio's window thread, the mouse's,
  // hellorio: xix's CML style): the loop, then a bone per case, each an
  // [anchor, role, say] (a case is code: `code:"| Key"` reaches it); the
  // cases go back to the loop
  select(name, loop, loop_role, cases):: {
    name: name,
    bones: [{ at: loop, role: loop_role }] + [{ at: c[0], role: c[1] } for c in cases],
    joints: [{ from: loop, to: c[0], say: c[2] } for c in cases]
            + [{ from: c[0], to: loop, say: 'again' } for c in cases],
  },

  // a module's own chain, its anchors in the file: [name, role, say]
  // each, a bare definition name (def:) in [file]
  module(file, name, steps)::
    self.chain(name, [[file + ':def:' + s[0], s[1], s[2]] for s in steps]),

  // a star: [center] calls each of [callees] ([anchor, role, say] each),
  // for a main calling ten initializations, a dispatcher its commands
  calls(name, center, center_role, callees):: {
    name: name,
    bones: [{ at: center, role: center_role }] + [{ at: c[0], role: c[1] } for c in callees],
    joints: [{ from: center, to: c[0], say: c[2] } for c in callees],
  },

  // a directory's parts as whole units (a path each, no anchor): a
  // layered picture, [path, role] each, and joints [from, to, say]
  parts(name, bones, joints):: {
    name: name,
    bones: [{ at: b[0], role: b[1] } for b in bones],
    joints: [{ from: j[0], to: j[1], say: j[2] } for j in joints],
  },
}
