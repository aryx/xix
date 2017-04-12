
type t = {
  mutable cnt: int;
  l: Spinlock.t;
}


let inc x =
  Spinlock.lock x.l;
  x.cnt <- x.cnt + 1;
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

  
    
