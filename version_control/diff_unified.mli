(*s: version_control/diff_unified.mli *)

(*s: signature Diff_unified.show_change *)
val show_change: Change.t -> unit
(*e: signature Diff_unified.show_change *)

(* internals *)
val show_unified_diff: Diff.diff -> unit
(*e: version_control/diff_unified.mli *)
