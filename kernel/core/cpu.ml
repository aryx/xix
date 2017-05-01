open Types

(* No need for Spinock.t here: this record is accessed by only 1 processor. *)

type t = {
  id: int; (* between 0 and Arch.max_cpus - 1 *)

  mutable proc: Proc_.t option;

  (* todo: Arch_cpu *)

  mutable ticks: t_ticks;
  (* less: cpumhz: int; *)

  thread: Thread.t;

  (* less: 
   * mutable ilock_depths: int;
   *)
}
