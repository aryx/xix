
type chunk =
  | Stable of Diff.item
  | ChangedA of Diff.item list (* Orig *) * Diff.item list (* A *)
  | ChangedB of Diff.item list (* Orig *) * Diff.item list (* B *)
  | FalseConflict of Diff.item list
  | TrueConflict of 
      Diff.item list (* Orig *) * Diff.item list (*A*) * Diff.item list (*B*)

val diff3: string (* Orig *) -> string (* A *) -> string (* B *) -> chunk list

val merge: string (* label A *) -> string (* label B *) -> chunk list -> string
