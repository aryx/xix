open Common
open Types
open Timer

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


let add timer () =
  raise Todo

let del timer () =
  raise Todo
