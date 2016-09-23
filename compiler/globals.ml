
(* Global line number (after pre-processing).
 * Note that you need another data structure to map a global line number 
 * to a (file, line) pair (see Preprocessor.line_history).
 *)
let line = ref 1

(* less:
 * nearln? or pass around in typechecking code?
 *)

let (htypedefs: (string, bool) Hashtbl.t) = 
  Hashtbl.create 101

(* outfile? for errorexit to delete outfile if there was any error *)
