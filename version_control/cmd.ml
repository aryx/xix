
(* alt: 
 *  - in ocaml-git, use of Term.() and $ to not require
 *    globals for flags?
*)
type t = {
  name: string;
  help: string;
  options: (Arg.key * Arg.spec * Arg.doc) list;
  f: string list -> unit;
  (* less: man: when do git -help get short help, and with --help man page *)
}

(* f can raise ShowUsage which will be catched by caller in main.ml *)
exception ShowUsage
