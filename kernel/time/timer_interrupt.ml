open Common
open Types

(* less: pass Ureg? *)
let timer_interrupt () =
  let cpu = Globals.cpu () in
  let timers = Timers.cpus_timers.(cpu.Cpu.id) in
  let now = Time.fastticks () in
  if now = 0
  then Error.panic "timerintr: zero fastticks";
  
  Ilock.lock timers.Timers.l;
  let rec aux hzclock =
    match timers.Timers.elts with
    | [] -> raise (Impossible "timers list empty, should contain hzclock timer")
    | timer::xs ->
      (* no Ilock.lock timer.l here! This could result in a deadlock.
       * The only dangerous operation is to delete the timer, 
       * with Timers.del and this must be done with a lock to timers held.
       * We have the timers lock, so Timers.del will wait until we're done
       *)
      let when_ = timer.Timer_.fasttk in
      if when_ > now
      then begin
        (* todo: arch_timerset *)
        timers.Timers.elts <- timer::xs;
        Ilock.unlock timers.Timers.l;
        if hzclock
        then Hz_clock.hz_clock ()
      end else begin
        assert (timer.Timer_.cpu = Some cpu.Cpu.id);
        timers.Timers.elts <- xs;
        (* timer is not active anymore *)
        timer.Timer_.cpu <- None;
        
        Ilock.unlock timers.Timers.l;
        (match timer.Timer_.f with
        | Timer_.HzClock -> 
          (* not yet. todo: why? *)
          ()
        | Timer_.Callback f ->
          f ()
        );
        Ilock.lock timers.Timers.l;
        if timer.Timer_.mode = Timer_.Periodic
        then begin
          let (xs, _new_head) = Timer.add timer timers.Timers.elts in
          (* no need check _new_head; the arch_timerset will happen eventually
           * later when when_ > now
           *)
          timers.Timers.elts <- xs
        end;
        aux (hzclock || timer.Timer_.f = Timer_.HzClock)
      end
  in
  aux false
