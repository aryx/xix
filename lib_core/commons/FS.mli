
val with_open_in : 
  <Cap.open_in; ..> -> (Chan.i -> 'a) -> Fpath.t -> 'a

val cat : <Cap.open_in; ..> -> Fpath.t -> string list
