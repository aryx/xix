
type env = {
  ids:  (string, Type.t * Storage.t) Hashtbl.t;
  tags: (string, Type.tagdef) Hashtbl.t;
  (* labels: string, ??  *)
  (* macros handled by cpp, so no need here *)
  
  block: blockid;

  ids_scope: string list list;
  tags_scope: string list list;
}
