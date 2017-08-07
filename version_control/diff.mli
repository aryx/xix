
type item = string

type diff_elem = 
  | Added of item
  | Deleted of item
  | Equal of item

type diff = diff_elem list

val diff: string -> string -> diff
