open Types

(* todo: PageOrSwap below? use Common.either? *)
type t = {
  (* opti: we use an array; faster than a list *)
  pagetab: Page_.t option array; (* length = pagetab_size = 256 *)

  (* opti: the fields below allow to avoid iterate over all entries in pagetab.
   * This is useful especially for SStack where allocated pages are at the end
   * of the array.
   * todo: but who needs to iterate? just Pagetable.free and copy?  useful 
   *  opti then? 
   *)
  mutable first: int; (* pagetab_size when pagetab is full of None *)
  mutable last: int;
}

let pagetab_size = 256
let pagetab_memory_mapped = pagetab_size * Memory.pg2by (* 1 MB *)
