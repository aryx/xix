open Common
open Types

(* A kind of portable Page Table Entry (PTE). It remembers also
 * the reverse mapping (pa -> va), include a ref count, and records other
 * meta-data about the page (modified/referenced).
 * 
 * less: opti: There will be lots of those Page.t so
 *   - factorize modified/references in a bitset
 *   - use int or short for reference count
 *   - just use MMU pte data structure for that (int storing lots of info)
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
