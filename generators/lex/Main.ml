(*s: lex/Main.ml *)

(*s: toplevel [[Main._1]] *)
let _ = 
  (* Deprecated in OCaml 5 cos done by default: Printexc.catch *)
  CLI.main (); 
  exit 0
(*e: toplevel [[Main._1]] *)
(*e: lex/Main.ml *)
