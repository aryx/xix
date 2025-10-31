open Types

(* Mostly a copy of Spinlock_.t with additional information about interrupts.
 * We could factorize with spinlock.ml as done in C and have
 * just a special field is_ilock: bool; in Spinlock_.t.
 * I prefer to separate the types because there are a few differences
 * like the lack of saved pid here. Also avoid extra sanity checks
 * like calling (spin)unlock on an ilock.
 *)
type t = {
  hold: bool ref;
  mutable saved_spl: Spl_.prio;

  (* less: debugging fields
   * pc: kern_addr;
   *)
}
