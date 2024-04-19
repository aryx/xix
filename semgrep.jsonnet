
local ocaml = import 'p/ocaml';

//Temporary hack to not report p/ocaml findings on semgrep libs
//TODO: we should use +: instead of :, as in
//  [ r + { paths +: { exclude +: [ "libs/*", "tools/*", "languages/*" ] } }
// but this is not supported yet by ojsonnet hence the use of :
local ocaml_rules =
  [
    r { paths: { exclude: ['libs/*', 'tools/*', 'languages/*'] } }
    for r in ocaml.rules
  ];

local semgrep_rules = [
  {
    // Just an example
    // TODO: actually we have lots of use of open_in which is why
    // I've renamed to open_in_TODO below but we should fix them!
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

// TODO? Use TCB rules?

local all = semgrep_rules;

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
