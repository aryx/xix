open Ref_

type t = Ref_.t



let inc x =
  Spinlock.lock x.l;
  x.cnt <- x.cnt + 1;
  (* todo: does this copy an int somewhere (e.g., in stack when in C) *)
  let v = x.cnt in
  Spinlock.unlock x.l;
  v

let dec x =
  Spinlock.lock x.l;
  x.cnt <- x.cnt - 1;
  let v = x.cnt in
  Spinlock.unlock x.l;
  if v < 0
  then failwith "Ref.dec has a negative count";
  v
