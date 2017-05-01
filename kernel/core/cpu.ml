open Types

(* No need for Spinock.t here: this record is accessed by only 1 processor. *)

type t = {
  cpuno: int;

  mutable proc: Proc_.t option;

  (* todo: Arch_cpu *)

  mutable ticks: t_ticks;
  (* less: cpumhz: int; *)

  thread: Thread.t;

  (* less: 
   * mutable ilock_depths: int;
   *)
}
