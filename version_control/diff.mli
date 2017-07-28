
type item = string

type diff_elem = 
  | Deleted of item
  | Added of item
  | Equal of item

type diff = diff_elem list

val diff: string -> string -> diff

