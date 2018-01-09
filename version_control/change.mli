(*s: version_control/change.mli *)

(*s: type [[Change.content]] *)
type content = bytes
(*e: type [[Change.content]] *)

(*s: type [[Change.entry]] *)
type entry = {
  (* relative path *)
  path: Common.filename;
  mode: Index.mode;
  content: content Lazy.t;
}
(*e: type [[Change.entry]] *)

(*s: type [[Change.t]] *)
(* entry below refers only to files (not dirs), and their name
 * are adjusted to show a relative path from the root of the
 * project.
 *)
type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry (* before / after *)
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
(*e: type [[Change.t]] *)
(*e: version_control/change.mli *)
