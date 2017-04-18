open Types

(* we can not put this type in concurrency/ because of mutual deps
 * between locks and a proc
 *)
type t = {
  hold: bool ref;

  (* less: opti: direct reference to Proc.t instead of pid *)
  mutable p: pid option;

  (* less: debugging fields
   * pc: kern_addr;
   *)

}
