(*s: version_control/change.mli *)

(*s: type Change.content (version_control/change.mli) *)
type content = bytes
(*e: type Change.content (version_control/change.mli) *)

(*s: type Change.entry (version_control/change.mli) *)
type entry = {
  path: Common.filename;
  mode: Index.mode;
  content: content Lazy.t;
}
(*e: type Change.entry (version_control/change.mli) *)

(*s: type Change.t (version_control/change.mli) *)
type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry
(*e: type Change.t (version_control/change.mli) *)
(*e: version_control/change.mli *)
