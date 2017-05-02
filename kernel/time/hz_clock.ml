open Common
open Types

(* (interrupt) -> ... -> <> *)
(* less: take a Ureg? *)
let hz_clock () =
  Globals.cpu.Cpu.ticks <- Globals.cpu.Cpu.ticks + 1;
  (* less: adjust Proc.pc? flushmmu? accountime? kproftimer? active? *)
  (* todo: check alarms *)
  let up = Globals.up () in
  if up.Proc_.state = Proc_.Running
  then Hz_sched.hz_sched ()

