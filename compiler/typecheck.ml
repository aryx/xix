(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* Environment for naming and typechecking.
 *
 * No macros here; they are handled by cpp. 
 *)
type env = {
  ids:  (string, Type.t * Storage.t) Hashtbl.t;
  tags: (string, Type.tagdef) Hashtbl.t;
  typedefs: (string, Ast.type_) Hashtbl.t;
  (* labels: string, ??  *)
  
  (* or do that in parsing? *)
  block: blockid;

  (* to push and pop when enter/leave a block or scope *)
  ids_scope: ((string * (Type.t * Storage.t)) list) list;
  tags_scope: ((string * Type.tagdef) list) list;
  typedefs_scope: ((string * Ast.type_) list) list;

  block_scope: blockid list;
}

(* todo:
 *  - check goto to a label defined in the function
 *)
