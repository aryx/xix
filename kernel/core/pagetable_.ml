open Types

(* todo: PageOrSwap below? use either? *)
type t = {
  (* opti: use an array; faster than a list *)
  pagetab: Page_.t option array; (* length = pagetab_size = 256 *)

  (* opti: to avoid iterate over all entries in pagetab.
   * useful for SStack where allocated pages are at the end
   * of the array.
   * todo: but who needs to iterate? just Pagetable.free and copy?
   *  useful opti then? 
   *)
  mutable first: int; (* pagetab_size when None *)
  mutable last: int;
}

let pagetab_size = 256
let pagetab_memory_mapped = pagetab_size * Memory.pg2by (* 1 MB *)
