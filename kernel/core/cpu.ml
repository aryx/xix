open Types

(* No need for Lock.t here: this structure is accessed by only 1 processor. *)

type t = {
  cpuno: int;

  proc: Proc_.t option ref;

  (* todo: Arch_cpu *)

  mutable ticks: int64;
  cpumhz: int;

  (* todo: stack? *)
  (* less: 
   * mutable ilock_depths: int;
   *)
}
