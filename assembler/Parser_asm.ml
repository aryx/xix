(*s: Parser_asm.ml *)
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

(*s: function [[Parser_asm.error]] *)
let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))
(*e: function [[Parser_asm.error]] *)

(*s: constant [[Parser_asm.noattr]] *)
(*e: constant [[Parser_asm.noattr]] *)
(*s: function [[Parser_asm.attributes_of_int]] *)
(* less: should use keywords in Asm5 instead of abusing integers
 * alt: anyway one can also use cpp to define NOPROF/DUPOK macros
 *)
let attributes_of_int i =
   match i with 
   | 0 -> default_attr
   (* NOPROF *)
   | 1 -> { dupok = false; no_prof = true }
   (* DUPOK *)
   | 2 -> { dupok = true; no_prof = false }
   (* both DUPOK and NOPROF *)
   | 3 -> { dupok = true; no_prof = true }

   | _ -> error (spf "unknown attribute or attribute combination: %d" i)
(*e: function [[Parser_asm.attributes_of_int]] *)

(*s: function [[Parser_asm.mk_e]] *)
let mk_e name static = 
  { name; priv = if static then Some (-1) else None; signature = None; }
(*e: function [[Parser_asm.mk_e]] *)
(*e: Parser_asm.ml *)
