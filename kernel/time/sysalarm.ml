open Common
open Types
open Alarms

let syscall_alarm ms_opt =
  let up = Globals.up () in
  (* less: use cpu0? why *)
  let cpu = Globals.cpu () in
  let remaining = 
    match up.Proc_.alarm with
    | Some x -> Time.tick_to_ms (x - cpu.Cpu.ticks)
    | None -> 0
  in
  match ms_opt with
  | None -> 
    up.Proc_.alarm <- None;
    remaining
  | Some ms ->
    let when_ = Time.ms_to_tick ms + cpu.Cpu.ticks in
    Qlock.lock Alarms.alarms.ql;
    raise Todo
