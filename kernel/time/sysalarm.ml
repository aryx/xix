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
    (* less: should remove it from Alarms *)
    up.Proc_.alarm <- None;
    remaining
  | Some ms ->
    let when_ = Time.ms_to_tick ms + cpu.Cpu.ticks in
    Qlock.lock Alarms.alarms.ql;
    Alarms.del_proc up;
    Alarms.add_proc up when_;
    Qlock.unlock Alarms.alarms.ql;
    remaining


let _init =
  Sysexits.hooks := (fun p -> 
    (* less: we should remove it also from Alarms.alarms *)
    p.Proc_.alarm <- None
  )::!Sysexits.hooks
