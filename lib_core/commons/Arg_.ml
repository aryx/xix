
(* no need Cap.exit because we raise ExitCode, we do not call Exit.exit() *)
let parse_argv (caps : < Cap.stdout; Cap.stderr; ..>) argv options f usage =
  try
    Arg.parse_argv argv options f usage
  with
  | Arg.Bad msg -> Console.eprint caps msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> Console.print caps msg; raise (Exit.ExitCode 0)
  
