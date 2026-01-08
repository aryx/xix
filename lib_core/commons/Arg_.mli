
(* small wrapper around Arg.parse_argv that also handles the Arg.Bad and Arg.Help
 * exns and use the Console and Exit modules to print help and raise ExitCode.
 *)
val parse_argv: < Cap.stdout; Cap.stderr; ..> ->
  string array -> (Arg.key * Arg.spec * Arg.doc) list -> Arg.anon_fun -> 
  Arg.usage_msg -> unit
