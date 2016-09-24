
(* todo?:
 *  - nearln? or pass around in typechecking code?
 *  - outfile? for errorexit to delete outfile if there was any error
 *)

let (htypedefs: (string, bool) Hashtbl.t) = 
  Hashtbl.create 101

