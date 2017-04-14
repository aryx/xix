open Types

(* todo: PageOrSwap below? use either? *)
type t = {
  (* Why not use a list? too slow! *)
  pagetab: Page.t option array; (* length = pagetab_size = 256 *)

  (* opti: to avoid iterate over all entries in pagetab *)
  mutable first: int; (* pagetab_size when None *)
  mutable last: int;
}

let pagetab_size = 256
let pagetab_memory_mapped = pagetab_size * Memory.pg2by (* 1 MB *)
