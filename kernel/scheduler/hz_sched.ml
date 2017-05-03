open Common
open Types
open Scheduler_

let any_higher () =
  raise Todo

(* (interrupt) -> ... -> hz_clock -> <> *)
let hz_sched () =
  let cpu = Globals.cpu () in
  if any_higher ()
     || (cpu.Cpu.ticks > cpu.Cpu.sched_ticks && Scheduler.any_ready())
  then begin
    (* less: cpu.readied <- None; *)
    (* todo? why not call sched() instead? *)
    let up = Globals.up () in
    (* TODO: up.Proc_.delay_sched <- up.Proc_.delay_sched + 1; *)
    raise Todo
  end
