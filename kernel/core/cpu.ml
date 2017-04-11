open Types

type t = {
  cpuno: int;

  proc: Proc.t option ref;

  (* todo: Arch_cpu *)

  mutable ticks: int64;
  cpumhz: int;

  (* less: stack? *)
}
