open Types

(* No need for Spinock.t here: this record is accessed by only 1 processor. *)

(* between 0 and Arch.max_cpus - 1 *)
type cpuid = Types.cpuid

type t = {
  id: cpuid; 

  mutable proc: Proc_.t option;

  (* todo: Arch_cpu *)

  mutable ticks: t_ticks;
  (* less: cpumhz: int; *)
  mutable sched_ticks: t_ticks;

  thread: Thread.t;

  (* less: 
   * mutable ilock_depths: int;
   *)
}
