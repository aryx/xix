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

  mutable refcnt: int; (* should be Ref.t but protected already by Page.l *)

  mutable modified: bool;
  mutable referenced: bool;

  (* less: color, cachectl *)

  l: Spinlock.t;
}
