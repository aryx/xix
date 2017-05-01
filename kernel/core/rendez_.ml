open Types

type t = {
  (* less: opti: direct Proc_.t reference *)
  mutable p: pid option;
  (* !lock ordering! r.l before p.rlock *)
  l: Spinlock_.t;
}
