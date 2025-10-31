open Common

(* No need for Spinock.t here: this record is accessed by only 1 processor. *)

(* between 0 and Arch.max_cpus - 1 *)
type cpuid = Types.cpuid

type t = {
  id: cpuid; 

  (* the scheduled proc on this processor! = Globals.up *)
  mutable proc: Process_.t option;

  mutable ticks: Types.t_ticks;
  (* at which next ticks the current process should be scheduled out *)
  mutable sched_ticks: Types.t_ticks;

  (* the main kernel thread (Thread.self() from main()) *)
  thread: Thread.t;

  (* todo: Arch_cpu
   * less: 
   *  - cpumhz: int;
   *  - could put Timers.timers here since one list per-cpu
   *  - mutable ilock_depths: int;
   *)
}
