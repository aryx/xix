open Common

(* As in ocaml source so easier for pretty printing *)
(* less: could spread in different directories *)
type error = 
  | Ebadarg
  | Enovmem
  | Esoverlap
  | Enochild
  | Ebadexec
exception Error of error

let error x = raise (Error x)

let panic str =
  raise (Impossible ("panic: " ^ str))
