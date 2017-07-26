
type t = 
  | Add of Tree.entry
  | Del of Tree.entry
  | Modify of Tree.entry * Tree.entry
