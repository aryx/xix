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
      Use `Common.with_open_infile()` or `Chan.with_open_in` instead.
    |||,
    paths: {
      exclude: ['common2.ml'],
    },
  },
];
// ----------------------------------------------------------------------------
// TCB
// ----------------------------------------------------------------------------
// partial copy of semgrep/TCB/forbid_xxx.jsonnet
local cap_rules = [
  {
    id: 'use-caps',
    match: { any:
        [
	 'Sys.chdir',
	 #'Unix.chdir'
	 'Unix.execve',
	 'Unix.execv',
	 'Unix.fork',
	 'Unix.environment',
	 #'Sys.getenv'
	]
    },
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
       Do not use Sys.xxx or Unix.xxx. Use CapSys or CapUnix and capabilities
       for dangerous functions.
    |||,
    paths: {
      exclude: ['CapSys.ml', 'CapUnix.ml', 'threadUnix.ml',
		'version_control/repository.ml',
		'windows/wm.ml',
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
