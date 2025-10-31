open Common
open Types

let kmap _page =
  raise Todo

let kunmap _page =
  raise Todo

(* less: let va k = ... ? 
 * or just duplicate the few memxxx function to take ka so
 * clearer interface and no need VA(k) as in C?
 *)

let memclear _ka _size =
  Logs.err (fun m -> m "TODO: Kernel_memory.memclean")
  (*raise Todo*)

