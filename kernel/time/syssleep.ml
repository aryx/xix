open Common
open Types

(* todo: can be interrupted !!! *)
let syscall_sleep (ms : Types.t_ms) : unit =
  (* stricter: *)
  match ms with
  | x when x < 0 -> failwith "sleep: negative time";
  | 0 -> Scheduler.yield ()
  | x ->
    (* sanitize *)
    let ms = 
      (* stricter? could warn the user *)
      if x < Time.tick_to_ms 1
      then Time.tick_to_ms 1
      else ms
    in
    let _up = Globals.up () in
    (* less: opti: reuse up.sleep_rdz instead of alloc each time? more complex*)
    let rdz = Rendez.alloc () in
    Time_rendez.sleep rdz (fun _ -> false) ms
