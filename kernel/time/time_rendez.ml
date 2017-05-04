open Common
open Types

let sleep rdz fcond ms =
  let up = Globals.up () in

  (* less: used to have a Proc_.timer option, but simpler allocate each time.
   * anyway, how can do double sleep? or exits while sleeping?
  (match up.Proc_.timer with
  (* less: check if inactive instead? of should never have Some because
   * we reset it back after? *)
  | Some _ ->
     (* stricter: could Timers.del it instead *)
     raise (Impossible "Time_rendez.sleep: timer already there");
  | None _ -> ()
  );
   *)
  let timer = Timer.alloc Timer_.Relative (Time.ms_to_ns ms)
    (fun _ -> Rendez.wakeup rdz |> ignore) in
  (* less: was mostly for sysexits, but when can do exits during a sleep?
   * up.Proc_.timer <- timer;
   *)
  Timers.add timer;
  (* less: pass by fn and tfn field?? *)
  (* todo: can have race between timer trigger and sleep? *)
  (* todo: Common.finalize that del timer if interrupted! *)
  Rendez.sleep rdz fcond;
  (* less: when used to have a Proc_.timer. check if active? when can happen?
   * up.Proc_.timer <- None; 
   *)
  (* todo: the waserror/nexterror/poperror? when can have error? *)
  ()
 
  

