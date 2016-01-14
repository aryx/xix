(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types

let check h =
  h |> Hashtbl.iter (fun (s, _) v ->
    match v.T.section with
    | T.SXref -> failwith (spf "%s: not defined" s)
    | _ -> ()
  )

(* less: could also check validity of object file:
 *  - registers are in range, 
 *  - integers are in range,
 *    ex with immediate in shifting operation:
 *    if i >= 0 && i <= 31
 *    then ...
 *    else failwith "shift value out of range"
 *  - that cond is AL for B and Bxx,
 *  - that use Local or Param only when inside a Text (and for Local
 *    that if fits the size specified),
 *    | None -> error loc "use of parameter outside of procedure"
 *    | None -> error loc "use of local outside of procedure"
 *  - ...
 *  See use of error() in codegen5.ml, or notes about 5l in ocaml in
 *  Linker.nw for more invariants to check.
 *)
