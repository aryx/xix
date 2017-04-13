open Common
open Types

(* A kind of portable Page Table Entry (PTE) .
 * 
 * less: optimize? There will be lots of those Page.t so
 *   - factorize modified/references in a bitfield
 *   - use int or short for reference count
 *)
type t = { 
  pa: phys_addr;
  mutable va: user_addr;

  mutable refcnt: Ref.t;
  mutable modified: bool;
  mutable referenced: bool;

  l: Spinlock.t;
}
