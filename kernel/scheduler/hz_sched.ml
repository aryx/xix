open Common
open Types
open Scheduler_

let any_higher () =
  raise Todo

(* run in interrupt context from hz_clock () *)
let hz_sched () =
  if any_higher ()
     || (Globals.cpu.Cpu.ticks > Globals.cpu.Cpu.sched_ticks && 
           Scheduler.any_ready())
  then begin
    (* less: cpu.readied <- None; *)
    (* todo? why not call sched() instead? *)
    let up = Globals.up () in
    (* TODO: up.Proc_.delay_sched <- up.Proc_.delay_sched + 1; *)
    raise Todo
  end
