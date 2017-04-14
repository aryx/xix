
type t = {
  mutable cnt: int;
  l: Spinlock_.t;
}
