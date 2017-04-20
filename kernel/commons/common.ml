
(* available by default since ocaml 4.01 but we use 1.07 for ocaml light *)
let (|>) o f = f o

(* We could use print_string below, but it requires some hacks to make
 * it work in a kernel context. Indeed, there is not really a
 * descriptor 0 for the kernel. So, it is better to make _print assignable
 * to a specific external C function (e.g., screenputs C).
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

(* found on stack overflow *)
let is_power_of_2 x =
  x <> 0 && 
  x land (x - 1) = 0 

(* a bit brute force ... *)
let log2h = Hashtbl.create 32
let _ = 
  Hashtbl.add log2h 0 0;
  Hashtbl.add log2h 2 1;
  Hashtbl.add log2h 4 2;
  Hashtbl.add log2h 8 3;
  Hashtbl.add log2h 16 4;
  Hashtbl.add log2h 32 5;
  Hashtbl.add log2h 64 6;
  Hashtbl.add log2h 128 7;
  Hashtbl.add log2h 256 8;
  Hashtbl.add log2h 512 9;
  Hashtbl.add log2h 1024 10;
  Hashtbl.add log2h 2048 11;
  Hashtbl.add log2h 4096 12;
  Hashtbl.add log2h 8192 13;
  (* todo: more *)
  ()

let log2 x =
  try 
    Hashtbl.find log2h x
  with Not_found -> failwith (spf "log2: %d is not a power of 2" x)

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
