open Types

(* A rendez-vous is a place where a process will sleep until someone
 * else wake him up.
 *)
type t = {
  (* less: opti: direct Proc_.t reference *)
  mutable p: pid option;
  (* !lock ordering! r.l before p.rlock *)
  (* todo: why need a lock on the rendezvous? because when want to sleep on it,
   * we dont want a wakeup to be lost?
   *)
  l: Spinlock_.t;
}
