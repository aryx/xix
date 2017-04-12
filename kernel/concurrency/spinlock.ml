open Common

(* todo:
 * - handle multiple processors and interrupts. Use arch_tas()!
 * less:
 * - use monitor approach instead of fine-grained locks?
 *)

type t = {
  mutable hold: bool;

}

(* simple when assume only 1 processor and no interrupt code *)
let lock x =
  if x.hold
  then failwith "lock already hold! Impossible if use only 1 processor"
  else x.hold <- true

let unlock x =
  if not x.hold
  then failwith "Spinlock.unlock: not locked";
  x.hold <- false

let with_lock f x =
  raise Todo
