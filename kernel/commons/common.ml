
let (|>) o f = f o

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

let roundup x pow2 =
  assert (is_power_of_2 pow2);
  (x + (pow2 - 1)) land (lnot (pow2 - 1))

(*TODO! let _ = assert(round_up 2045 1024 = 3072) *)

let finalize f cleanup =
  try
    let res = f () in
    cleanup ();
    res
  with e ->
    cleanup ();
    raise e
