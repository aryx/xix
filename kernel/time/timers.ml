open Common
open Types

type t = {
  (* sorted by fasttk *)
  mutable elts: Timer.t list;

  (* !lock ordering! lock(Timer.t); lock(Timers.t) *)
  l: Ilock.t;
}

let alloc () = {
  elts = [];
  l = Ilock.alloc ();
}


let cpus_timers = 
  Array.init Arch.max_cpus (fun _ -> alloc ())


let add timer =
  Ilock.lock timer.Timer_.l;

  (* stricter? allow to have timer already active *)
  timer.Timer_.cpu |> Common.if_some (fun cpuid ->
    (* timerdel? but then should call arch_timerset somewhere?
     * or assert cpuid = cpu.id?
     * or ok if timer occurs anyway; it will do not trigger any callback.
    *)
    raise Todo 
  );
  assert(timer.Timer_.cpu = None);

  let cpu = Globals.cpu () in
  let timers = cpus_timers.(cpu.Cpu.id) in
  Ilock.lock timers.l;
  let (xs, new_head) = Timer.add timer timers.elts in
  new_head |> Common.if_some (fun fasttk ->
    Arch.timerset fasttk
  );
  timer.Timer_.cpu <- Some cpu.Cpu.id;
  timers.elts <- xs;
  Ilock.unlock timers.l;
  Ilock.unlock timer.Timer_.l;
  ()

let del timer =
  Ilock.lock timer.Timer_.l;

  match timer.Timer_.cpu with
  | None -> raise (Impossible "del: should not be called on inactive timer")
  | Some cpuid ->
    let timers = cpus_timers.(cpuid) in
    Ilock.lock timers.l;
    (* todo: can have race where timer not anymore in elts
     * because timer_interrupt removed it?
     *)
    let (xs, new_head) = Timer.del timer timers.elts in
    new_head |> Common.if_some (fun fasttk ->
      let cpu = Globals.cpu () in
      if cpuid = cpu.Cpu.id
      then Arch.timerset fasttk
    );
    (* todo: update its Proc_.timer to None? *)
    timer.Timer_.cpu <- None; 
    timers.elts <- xs;
    Ilock.unlock timers.l;
    Ilock.unlock timer.Timer_.l;
    ()

(* todo: used to a Proc_.timer option, but how can exits while sleeping?
 * todo: can exits when killed by note from someone else!
 * simpler to allocate process timer in each Time_rendez.sleep
let _init =
  Sysexits.hooks := (fun p -> 
    p.Proc_.timer |> Common.if_some (fun timer ->
      (* todo: could be inactive? because already ellapsed? *)
      del timer
    )
  )::!Sysexits.hooks
*)

let add_clock0_periodic_timer f ms =
  (* less: sanity check timersinit *)
  let ms = 
    if ms = 0
    then Time.tick_to_ms 1
    else ms
  in
  let timer = Timer.alloc Timer_.Periodic (Time.ms_to_ns ms) f in
  (* mostly copy paste of end of add(), but no need lock timer, and use cpu0 *)
  let timers = cpus_timers.(0) in
  Ilock.lock timers.l;
  let (xs, new_head) = Timer.add timer timers.elts in
  new_head |> Common.if_some (fun fasttk ->
    Arch.timerset fasttk
  );
  timer.Timer_.cpu <- Some 0;
  timers.elts <- xs;
  Ilock.unlock timers.l;
  timer

