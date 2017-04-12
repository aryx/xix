open Common

(* todo: external arch_tas: bool ref -> bool = "caml_tas" *)

(* tas() get the old content, set the ref, and then return
 * the old content.
 * 
 * todo: for now no concurrency in ocaml so pretty simple ...
 *)
let tas boolref =
  boolref := true;
  false
