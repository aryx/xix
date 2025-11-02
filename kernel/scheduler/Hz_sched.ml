open Common
open Types
open Scheduler_

let any_higher () =
  raise Todo

(* (interrupt) -> ... -> hz_clock -> <> *)
let hz_sched () : unit =
  let cpu : Cpu.t = Globals.cpu () in
  if any_higher ()
     || (cpu.ticks > cpu.sched_ticks && Scheduler.any_ready())
  then begin
    (* less: cpu.readied <- None; *)
    (* todo? why not call sched() instead? *)
    let _up = Globals.up () in
    (* TODO: up.Proc_.delay_sched <- up.Proc_.delay_sched + 1; *)
    raise Todo
  end
