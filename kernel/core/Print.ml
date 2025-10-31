open Common

(* We could use print_string below, but it requires some hacks to make
 * it work in a kernel context. Indeed, there is not really a
 * descriptor 0 for the kernel. So, it is better to make _print assignable
 * to a specific external C function (e.g., screenputs C).
 *)
let (_print: (string -> unit) ref) = 
  ref (fun _s -> failwith "Common._print not defined")
let print s = !_print s
