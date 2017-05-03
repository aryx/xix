open Common
open Types

type t = {
  (* sorted by fasttk *)
  mutable elts: Timer.t list;
  l: Ilock.t;
 }

let alloc () = {
  elts = [];
  l = Ilock.alloc ();
}


let cpu_timers = 
  Array.init Arch.max_cpus (fun _ -> alloc ())


let add timer =
  Ilock.lock timer.Timer_.l;

  (* stricter? allow to have timer already active *)
  timer.Timer_.cpu |> Common.if_some (fun cpuid ->
    (* timerdel? but then should call arch_timerset somewhere?
     * or assert cpuid = cpu.id ?
    *)
    raise Todo 
  );
  assert(timer.Timer_.cpu = None);

  let cpu = Globals.cpu () in
  let timers = cpu_timers.(cpu.Cpu.id) in
  Ilock.lock timers.l;
  let (xs, new_head) = Timer.add timer timers.elts in
  new_head |> Common.if_some (fun fast ->
    raise Todo (* arch_timerset *)
  );
  timers.elts <- xs;
  Ilock.unlock timers.l;
  Ilock.unlock timer.Timer_.l;
  ()

let del timer =
  Ilock.lock timer.Timer_.l;

  match timer.Timer_.cpu with
  | None -> raise (Impossible "del: should not call del on inactive timer")
  | Some cpuid ->
    let timers = cpu_timers.(cpuid) in
    Ilock.lock timers.l;
    let (xs, new_head) = Timer.del timer timers.elts in
    new_head |> Common.if_some (fun fast ->
      let cpu = Globals.cpu () in
      if cpuid = cpu.Cpu.id
      then raise Todo (* arch_timerset *)
    );
    timers.elts <- xs;
    Ilock.unlock timers.l;
    Ilock.unlock timer.Timer_.l;
    ()

