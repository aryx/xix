// Semgrep rules for XiX, mostly to enforce the use of capabilities
// (see lib_core/commons/Cap.ml).

// ----------------------------------------------------------------------------
// Simple rules
// ----------------------------------------------------------------------------

local semgrep_rules = [
  {
    // Just an example
    id: 'no-open-in',
    match: { any: ['open_in_bin ...', 'open_in_TODO ...'] },
    // Same but using The old syntax:
    //  "pattern-either": [
    //    { pattern: "open_in_bin ..." },
    //    { pattern: "open_in ..." },
    //   ],
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
      It is easy to forget to close `open_in` with `close_in`.
      Use `FS.with_open_in()` (or `UChan.with_open_in`) instead.
    |||,
    paths: {
      exclude: ['todo/'],
    },
  },
];
// ----------------------------------------------------------------------------
// TCB (Trusted Computing Base, see semgrep/TCB/ for more info)
// ----------------------------------------------------------------------------

// lex and yacc are also part of ocaml-light so better not impose caps there
local exclude_dirs = ['lex/', 'yacc/', 'vcs', 'todo/'];

// partial copy of semgrep/TCB/forbid_xxx.jsonnet
local cap_rules = [
  {
    id: 'use-caps',
    match: { any:
        [
        # Cap.exit: see separate rule
        # 'exit',
        # Cap.argv: see separate rule
        # 'Sys.argv',
	# Cap.chdir
	 'Sys.chdir',
	 'Unix.chdir',
        # Cap.forkew
         'Sys.command',
         'Unix.system',
        # Cap.exec
	 'Unix.execve',
	 'Unix.execv',
	# Cap.fork
	 'Unix.fork',
	# Cap.wait
	 'Unix.wait',
	 'Unix.waitpid',
	# Cap.kill
         'Unix.kill',
	# Cap.env
	 'Unix.environment',
	 'Sys.getenv',
	 'Sys.getenv_opt',
	# Cap.open_in: see separate rule
	 'open_in_bin',
	 #'open_in',
         #'Unix.openfile',
         'UChan.with_open_in',
	# Cap.open_out
        'open_out',
        'UChan.with_open_out',
	# Cap.open_out
        'Sys.remove',
        'Unix.unlink',
        # print_string, print_int, print_bool, ...
        # Cap.tmp
        'Filename.temp_file',
	]
    },
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use Sys.xxx or Unix.xxx (or UChan.xxx). Use CapSys or CapUnix and
       capabilities for dangerous functions.
    |||,
    paths: {
      exclude: ['threadUnix.ml',
	        'Filename.ml', # for Sys.getenv TMPDIR, and temp_file
		] + exclude_dirs,
    },
  },
  {
    id: 'do-not-use-exit',
    # Cap.exit
    match: 'exit $N',
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use exit. Use CapStdlib.exit and capabilities.
    |||,
    paths: {
      exclude: [
        'ksym.ml', 'thread.ml', 'Printexc.ml', 'threadUnix.ml',
	'Arg.ml', 'Unix.ml',
      ] + exclude_dirs,
    },
  },
  {
    id: 'do-not-use-argv',
    # Cap.argv
    match: 'Sys.argv',
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use Sys.argv. Use CapSys.argv and capabilities.
    |||,
    paths: {
      exclude: ['Arg.ml', 'Unix.ml'] + exclude_dirs,
    },
  },
  {
    id: 'do-not-use-open-in',
    # Cap.open_in
    match: 'open_in',
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use open_in. Use FS.with_open_in and capabilities.
    |||,
    paths: {
      exclude: [] + exclude_dirs,
    },
  },
  {
    id: 'do-not-use-obj-magic',
    # Cap.open_in
    match: 'Obj.magic',
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use Obj.magic!
    |||,
    paths: {
      exclude: [ 
	"lib_core/commons/Dumper.ml", 
	"lib_core/misc/camlinternalOO.ml",
	"lib_core/parsing/Parsing.ml",
	"lib_core/printing/Format.ml",
	"lib_core/printing/Printexc.ml",
	"lib_core/printing/Printf.ml",
	"lib_parsing/Lexing_.ml",
	"lib_parsing/Parsing_.ml",
        ] + exclude_dirs,
    },
  },
];
							   
// ----------------------------------------------------------------------------
// Skip and last-minute override
// ----------------------------------------------------------------------------

local todo_skipped_for_now = [
  //TODO? what is the fix for that?
  'ocaml.lang.portability.crlf-support.broken-input-line',
  // too noisy
  'ocaml.lang.security.hashtable-dos.ocamllint-hashtable-dos',
];

local override_messages = {
  // Semgrep specific adjustments
  'ocaml.lang.best-practice.exception.bad-reraise': |||
    You should not re-raise exceptions using 'raise' because it loses track
    of where the exception was raised originally. See commons/Exception.mli
    for more information.
    Use `Exception.catch exn` and later `Exception.raise exn` or
    `Exception.catch_and_reraise exn` if there is no code between the moment
    you catch the exn and re-raise it.
  |||,
};

// ----------------------------------------------------------------------------
// Entry point
// ----------------------------------------------------------------------------

local all = semgrep_rules + cap_rules;
{
  rules:
    [
      if std.objectHas(override_messages, r.id)
      then (r { message: override_messages[r.id] })
      else r
      for r in all
      if !std.member(todo_skipped_for_now, r.id)
    ],
}
