(*s: version_control/cmd_.ml *)

(*s: type [[Cmd_.t]] *)
type t = {
  name: string;
  usage: string;
  options: (Arg.key * Arg.spec * Arg.doc) list;

  (* the command! *)
  f: string list -> unit;
  (* less: man: when do git -help get short help, and with --help man page *)
}
(*e: type [[Cmd_.t]] *)

(*s: exception [[Cmd_.ShowUsage]] *)
(* Cmd_.f can raise ShowUsage. It will be catched by Main.main *)
exception ShowUsage
(*e: exception [[Cmd_.ShowUsage]] *)
(*e: version_control/cmd_.ml *)
