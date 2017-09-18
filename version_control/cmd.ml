(*s: version_control/cmd.ml *)

(*s: type Cmd.t *)
type t = {
  name: string;
  help: string;
  options: (Arg.key * Arg.spec * Arg.doc) list;

  (* the command! *)
  f: string list -> unit;
  (* less: man: when do git -help get short help, and with --help man page *)
}
(*e: type Cmd.t *)

(*s: exception Cmd.ShowUsage *)
(* f can raise ShowUsage which will be catched by caller in main.ml *)
exception ShowUsage
(*e: exception Cmd.ShowUsage *)
(*e: version_control/cmd.ml *)
