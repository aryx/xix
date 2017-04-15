open Types

type t = {
  hold: bool ref;
  (* debugging fields *)

  (* less: opti: direct reference to Proc.t instead of pid *)
  mutable p: pid option;
  (* less:
   * pc: kern_addr;
   *)

}
