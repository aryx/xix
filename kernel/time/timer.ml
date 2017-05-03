open Common
open Types
open Timer_

type t = Timer_.t

let alloc mode ns f =
  { mode = mode;
    ns = ns;
    fasttk = 0;
    f = Callback f;
    cpu = None;
    l = Ilock.alloc ();
  }

let add timer xs =
  (* less: assert timer is locked? *)

  (match timer.mode with
  | Relative -> 
    (* stricter: *)
    if timer.ns <= 0
    then failwith "timer going in the past";
    timer.fasttk <- Time.fastticks () + Time.ns_to_fastticks timer.ns;
  | Periodic ->
    if timer.ns < 100000
    then failwith "Periodic timer must be at least 100 micro seconds";
    (* less: combine with identical timer *)
    timer.fasttk <- Time.fastticks () + Time.ns_to_fastticks timer.ns;
  );
  (* insert in sorted list *)
  raise Todo

let del timer xs =
  raise Todo
