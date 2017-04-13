
type t = {
  mutable cnt: int;
  l: Spinlock.t;
}
