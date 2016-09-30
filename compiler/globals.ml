(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* See also globals in ../macroprocessor/location_cpp.ml  *)

(* todo?:
 *  - nearln? or pass around in typechecking code?
 *  - outfile? for errorexit to delete outfile if there was any error
 *)

(* mostly to recognize typedefs in the lexer *)
let (hids: (string, Ast.idkind) Hashtbl.t) = 
  Hashtbl.create 101
