
(* Global line number, after pre-processing. Need a special
 * structure to map a global line number to a (file, line) pair.
 *)
let line = ref 1

(* less:
 * nearln? or pass around in typechecking code?
 *)

let (htypedefs: (string, bool) Hashtbl.t) = Hashtbl.create 101
