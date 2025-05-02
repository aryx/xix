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
      Use `CapFS.with_open_in()` or `UChan.with_open_in` instead.
    |||,
    paths: {
      exclude: ['common2.ml'],
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
	# Cap.chdir
	 'Sys.chdir',
	 'Unix.chdir',
        # Cap.exec
	 'Unix.execve',
	 'Unix.execv',
	# Cap.fork
	 'Unix.fork',
	# Cap.env
	 'Unix.environment',
	 #'Sys.getenv',
	# Cap.open_in
	 'open_in_bin',
	 #'open_in',
	 #'UChan.with_open_in',
	 #'UChan.with_open_out',
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
		'version_control/repository.ml',
		'windows/wm.ml', 'windows/processes_winshell.ml'
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
        'compiler/error.ml', 'ksym.ml', 'thread.ml', 'printexc.ml', 'threadUnix.ml',
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
