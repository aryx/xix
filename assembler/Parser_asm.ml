open Common

open Ast_asm
module L = Location_cpp

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
