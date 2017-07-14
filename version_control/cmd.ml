
(* alt: 
 *  - in ocaml-git, use of Term.() and $ to not require
 *    globals for flags?
*)
type t = {
  name: string;
  help: string;
  options: (Arg.key * Arg.spec * Arg.doc) list;
  f: string list -> unit;
}
