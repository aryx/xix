(*s: Globals.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* See also globals in ../macroprocessor/location_cpp.ml  *)

(* to recognize typedefs in the lexer *)
let (hids: (string, Ast.idkind) Hashtbl.t) = 
  Hashtbl.create 101
(*e: Globals.ml *)
