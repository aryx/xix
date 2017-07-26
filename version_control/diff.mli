
type item = string

type diff_elem = 
  | Deleted of item list
  | Added of item list
  | Equal of item list

type diff = diff_elem list

val diff: string -> string -> diff

