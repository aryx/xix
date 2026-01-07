(*s: In.mli *)

(*s: signature [[In.newline]] *)
(* check the next token is a newline (or EOF) and consume it *)
val newline: Env.t -> unit
(*e: signature [[In.newline]] *)

(*s: signature [[In.filename]] *)
(* read a filename from stdin or from Env.savedfile otherwise *)
val filename: Env.t -> char (* 'f' or 'e' or ? *) -> Fpath.t
(*e: signature [[In.filename]] *)

(* return a line (without trailing '\n') *)
val gety :  Env.t -> string

(*s: signature [[In.gettty]] *)
(* Used to read a set of lines from stdin until a single "." on a line
 * is entered marking the end of user text input.
 *)
val gettty : Env.t -> (unit -> string option)
(*e: signature [[In.gettty]] *)
(*e: In.mli *)
