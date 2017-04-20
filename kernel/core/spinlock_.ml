open Types

(* we can not put this type in concurrency/ because of mutual deps
 * between locks and a proc
 *)
type t = {
  hold: bool ref;

  (* debugging and defensive programming fields *)

  (* less: opti: direct reference to Proc.t instead of pid *)
  (* really 'pid option' but for init kernel code we use the pid 0, the
   * one in Globals.fakeproc assigned initially to Globals.up.
   *)
  mutable p: pid;

  (* less: debugging fields
   * pc: kern_addr;
   *)

}
