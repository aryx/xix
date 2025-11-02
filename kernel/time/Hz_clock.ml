open Common
open Types

(* (interrupt) -> ... -> <> *)
(* less: take a Ureg? *)
let hz_clock () : unit =
  let cpu : Cpu.t = Globals.cpu () in
  (* incremented Arch.hz times per second *)
  cpu.ticks <- cpu.ticks + 1;

  (* less: adjust Proc.pc? flushmmu? accountime? kproftimer? active? *)
  (* todo: check alarms *)

  let up = Globals.up () in
  if up.state = Process_.Running
  then Hz_sched.hz_sched ()
