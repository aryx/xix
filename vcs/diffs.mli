(*s: version_control/diffs.mli *)

(*s: signature [[Diffs.diff]] *)
val diff: string -> string -> Diff.diff
(*e: signature [[Diffs.diff]] *)

(* the split lines contain the trailing '\n' when they have one *)
val split_lines: string -> string list

(*e: version_control/diffs.mli *)
