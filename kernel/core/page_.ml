open Common
open Types

(* The type below is a kind of portable Page Table Entry (PTE). 
 * It maps a virtual address to a physical address.
 * 
 * Page.t remembers also the reverse mapping (pa -> va), includes 
 * a ref count, and records other meta-data about the page 
 * (modified/referenced).
 * 
 * less: opti: there will be lots of those Page.t so we should:
 *   - factorize modified/references in a bitset
 *   - use int or short for reference count
 *   - just use MMU pte data structure for that 
 *     (an int can store lots of info because the 12 low bits of the PTE
 *      are not used because the physical address must be at a page boundary)
 *)
type t = { 
  (* the PTE *)
  pa: phys_addr;

  mutable va: user_addr;

  mutable refcnt: int; (* should be Ref.t but protected already by Page.l *)
  
  (* PTE bitfields *)
  mutable modified: bool;
  mutable referenced: bool;

  (* less: color, cachectl *)

  (* acts also as Ref.t via refcnt above *)  
  l: Spinlock_.t;
}
