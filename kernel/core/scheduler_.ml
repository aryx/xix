open Common

(* less: count EDF real-time priorities *)
let nb_priorities = 20

(* between 0 and nb_priorities - 1; higher means higher priority *)
type priority = Prio of int

let prioNormal = Prio 10
let prioKproc = Prio 13
let prioRootProcess = Prio 13
(* less: EDF priorities *)

(* The function below is necessary in C because C has no array bound-checking. 
 * In OCaml it is less important.
 *)
let mk_prio n =
  if n < 0 || n >= nb_priorities
  (* stricter: 9 just sanitize by clamping *)
  then failwith (spf "wrong priority: %d" n)
  else Prio n
