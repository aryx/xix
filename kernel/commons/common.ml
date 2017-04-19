
(* available in ocaml 3.0x? by default but we use ocaml 1.07 for ocaml light *)
let (|>) o f = f o

(* We could use print_string below, but it requires some hacks to make
 * it work in a kernel context. Indeed, there is no really a
 * descriptor 0 for the kernel. So, it is better to make _print assignable
 * to a specific external C function (e.g., screenputs).
 *)
let (_print: (string -> unit) ref) = 
  ref (fun s -> failwith "Common._print not defined")
let print s = !_print s

let spf = Printf.sprintf

let if_some f = function
  | None -> ()
  | Some x -> f x

exception Todo
exception Impossible of string

let is_power_of_2 x =
  x <> 0 && 
  x land (x - 1) = 0 

let log2 x =
  if false
  then 0
  else raise Todo

let roundup x pow2 =
  assert (is_power_of_2 pow2);
  (x + (pow2 - 1)) land (lnot (pow2 - 1))

(*TODO! let _ = assert(round_up 2045 1024 = 3072) *)

(* useful for the with_lock functions! no need waserror/nexterror/poperror *)
let finalize f cleanup =
  try
    let res = f () in
    cleanup ();
    res
  with e ->
    cleanup ();
    raise e
