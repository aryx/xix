(*s: Check.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Types
module T = Types

(*s: function [[Check.check]] *)
let check h =
  h |> Hashtbl.iter (fun symb v ->
    match v.section with
    | SXref -> failwith (spf "%s: not defined" (T.s_of_symbol symb))
    | _ -> ()
  )
(*e: function [[Check.check]] *)

(* todo: could also check validity of object file:
 *  - no duplicate DATA for same global.
 *    for instance with int foo = 1; int foo = 2;  5c does not say anything,
 *    but we should warn at least in linker because generated assembly
 *    contains DATA concerning same global!
 * less:
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
 * 
 *  See use of error() in codegen5.ml, or notes about 5l in ocaml in
 *  Linker.nw for more invariants to check.
 *)
(*e: Check.ml *)
