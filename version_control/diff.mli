(*s: version_control/diff.mli *)

(*s: type [[Diff.item]] *)
type item = string
(*e: type [[Diff.item]] *)

(*s: type [[Diff.diff_elem]] *)
(* similar to change.ml, but for content of the file *)
type 'item diff_elem = 
  | Added   of 'item
  | Deleted of 'item
  | Equal   of 'item
(*e: type [[Diff.diff_elem]] *)

(*s: type [[Diff.diff]] *)
type diff = (item diff_elem) list
(*e: type [[Diff.diff]] *)

(*e: version_control/diff.mli *)
