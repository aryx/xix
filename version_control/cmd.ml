(*s: version_control/cmd.ml *)

(*s: type [[Cmd.t]] *)
type t = {
  name: string;
  usage: string;
  options: (Arg.key * Arg.spec * Arg.doc) list;

  (* the command! *)
  f: string list -> unit;
  (* less: man: when do git -help get short help, and with --help man page *)
}
(*e: type [[Cmd.t]] *)

(*s: exception [[Cmd.ShowUsage]] *)
(* Cmd.f can raise ShowUsage. It will be catched by Main.main *)
exception ShowUsage
(*e: exception [[Cmd.ShowUsage]] *)
(*e: version_control/cmd.ml *)
