open Common
open Types

(* A kind of portable Page Table Entry (PTE), but remember also
 * reverse mapping (pa -> va), include a ref count, and other
 * meta-data about the page (modified/referenced).
 * 
 * less: optimize? There will be lots of those Page.t so
 *   - factorize modified/references in a bitfield
 *   - use int or short for reference count
 *)
type t = { 
  (* the PTE *)
  pa: phys_addr;

  mutable va: user_addr;

  mutable refcnt: int; (* should be Ref.t but protected already by Page.l *)

  mutable modified: bool;
  mutable referenced: bool;

  (* less: color, cachectl *)

  l: Spinlock_.t;
}
