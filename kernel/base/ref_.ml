
(* we can not put this type in concurrency/ because of mutual deps
 * between refs and a proc
 *)
type t = {
  mutable cnt: int;
  l: Spinlock_.t;
}
