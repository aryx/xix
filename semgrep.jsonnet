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
      Use `FS.with_open_in()` or `UChan.with_open_in` instead.
    |||,
    paths: {
      exclude: [],
    },
  },
];
// ----------------------------------------------------------------------------
// TCB (Trusted Computing Base, see semgrep/TCB/ for more info)
// ----------------------------------------------------------------------------
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
	 #'UChan.with_open_in',
	 #'UChan.with_open_out',
	# Cap.open_out
         #'open_out',
        # Cap.tmp
         # Filename.temp_file
	]
    },
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use Sys.xxx or Unix.xxx. Use CapSys or CapUnix and capabilities
       for dangerous functions.
    |||,
    paths: {
      exclude: ['threadUnix.ml',
	        'lib_core/strings/todo/filename.ml', # for Sys.getenv TMPDIR
		'version_control/repository.ml',
		'windows/wm.ml', 'windows/processes_winshell.ml',
		
		],
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
        'ksym.ml', 'thread.ml', 'printexc.ml', 'threadUnix.ml',
	'lib_system/arg.ml', 'lib_system/unix/unix.ml',
        'lex/', 'yacc/', 'version_control/',
	'windows/'
      ],
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
      exclude: ['lib_system/arg.ml', 'lib_system/unix/unix.ml',
         'lex/', 'yacc/', 'version_control/'
      ],
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
      exclude: [
         'lib_core/commons_plan9/', 'lib_graphics/draw/draw_rio.ml',
         'lex/', 'yacc/', 'version_control/'
      ],
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
