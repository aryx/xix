(*s: version_control/diff.mli *)

(*s: type Diff.item *)
type item = string
(*e: type Diff.item *)

(*s: type Diff.diff_elem *)
(* similar to change.ml, but for content of the file *)
type diff_elem = 
  | Added of item
  | Deleted of item
  | Equal of item
(*e: type Diff.diff_elem *)

(*s: type Diff.diff *)
type diff = diff_elem list
(*e: type Diff.diff *)


(*s: signature Diff.diff *)
val diff: string -> string -> diff
(*e: signature Diff.diff *)
(*e: version_control/diff.mli *)
