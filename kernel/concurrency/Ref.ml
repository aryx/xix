open Common

open Types
open Ref_

type t = Ref_.t

let alloc (n : int) : t = 
  { cnt = n;
    l = Spinlock.alloc ();
  }


let inc (x : t) : int =
  Spinlock.lock x.l;
  (* less: can detect overflow? exn in ocaml? *)
  x.cnt <- x.cnt + 1;
  (* todo: does this copy an int somewhere (e.g., in stack when in C) *)
  let v = x.cnt in
  Spinlock.unlock x.l;
  v

let dec (x : t) : int =
  Spinlock.lock x.l;
  x.cnt <- x.cnt - 1;
  let v = x.cnt in
  Spinlock.unlock x.l;
  if v < 0
  then failwith "Ref.dec has a negative count";
  v

let dec_and_is_zero (x : t) : bool =
  Spinlock.lock x.l;
  x.cnt <- x.cnt - 1;
  let v = x.cnt in
  Spinlock.unlock x.l;
  if v < 0
  then failwith "Ref.dec has a negative count";
  v = 0

  

(* a Ref is often used as a lock for other fields (e.g., in Page_.t) *)
let lock (x : t) : unit =
  Spinlock.lock x.l
let unlock (x : t) : unit =
  Spinlock.unlock x.l
