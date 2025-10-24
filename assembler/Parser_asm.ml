open Common

open Ast_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers used in the different arch-specific parsers.
 *
 * General limitations compared to 5a/va/...:
 *  - no support for 'NAME=expr;' constant definition, nor use of it
 *    in expressions, which allows to evaluate constant at parsing time 
 *    and avoid the need to build an expr AST (as we want to separate AST
 *    generation from checks/resolution/eval)
 *    (but can use cpp for constant definition so not a big loss)
 *  - does not allow SP and PC in a few places with 'spreg' and 'sreg' original
 *    grammar rule
 *    (but can use directly R13 and R15)
 *  - no END instruction
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))

let noattr = { dupok = false; prof = true }

(* less: should use keywords in Asm5 instead of abusing integers *)
let attributes_of_int i =
   match i with 
   | 0 -> noattr
   (* NOPROF *)
   | 1 -> { dupok = false; prof = false }
   (* DUPOK *)
   | 2 -> { dupok = true; prof = true }
   (* both DUPOK and NOPROF *)
   | 3 -> { dupok = true; prof = false }

   | _ -> error (spf "unknown attribute or attribute combination: %d" i)

let mk_e name static = 
  { name; priv = if static then Some (-1) else None; signature = None; }
