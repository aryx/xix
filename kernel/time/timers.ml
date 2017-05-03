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


let timers = 
  Array.init Arch.max_cpus (fun _ -> alloc ())


let add timer =
  Ilock.lock timer.Timer_.l;
  timer.Timer_.cpu |> Common.if_some (fun cpuid ->
    raise Todo (* timerdel *)
  );
  let cpu = Globals.cpu () in
  let t = timers.(cpu.Cpu.id) in
  Ilock.lock t.l;
  let (xs, new_head) = Timer.add timer t.elts in
  new_head |> Common.if_some (fun fast ->
    raise Todo (* arch_timerset *)
  );
  Ilock.unlock t.l;
  Ilock.unlock timer.Timer_.l;
  ()

let del timer () =
  raise Todo
