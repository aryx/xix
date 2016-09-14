
(* for scope *)
type blockid = int

type env = {
  ids: (string, Type.t * Storage.t) Hashtbl.t;
  tags: (string, Type.tagdef) Hashtbl.t;
  (* labels: string, ??  *)
  (* macros handled by cpp, so no need here *)
}
