(*s: version_control/diff_myers.mli *)
(**
   An implementation of Eugene Myers' O(ND) Difference Algorithm\[1\].
   This implementation is a port of util.lcs module of
   {{:http://practical-scheme.net/gauche} Gauche Scheme interpreter}.

   - \[1\] Eugene Myers, An O(ND) Difference Algorithm and Its Variations, Algorithmica Vol. 1 No. 2, pp. 251-266, 1986.
 *)

(*s: signature [[Diff_myers.diff]] *)
val diff: string array -> string array -> Diff.diff
(*e: signature [[Diff_myers.diff]] *)
(*e: version_control/diff_myers.mli *)
