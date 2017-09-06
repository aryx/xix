(*s: version_control/diff.mli *)

(*s: type Diff.item (version_control/diff.mli) *)
type item = string
(*e: type Diff.item (version_control/diff.mli) *)

(*s: type Diff.diff_elem (version_control/diff.mli) *)
type diff_elem = 
  | Added of item
  | Deleted of item
  | Equal of item
(*e: type Diff.diff_elem (version_control/diff.mli) *)

(*s: type Diff.diff (version_control/diff.mli) *)
type diff = diff_elem list
(*e: type Diff.diff (version_control/diff.mli) *)

(*s: signature Diff.diff *)
val diff: string -> string -> diff
(*e: signature Diff.diff *)
(*e: version_control/diff.mli *)
