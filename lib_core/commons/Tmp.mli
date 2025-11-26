
val new_file: <Cap.tmp; ..> -> 
  string (* prefix *) -> Fpath.ext (* suffix *) -> Fpath.t

val with_new_file: <Cap.tmp; ..> ->
  string (* prefix *) -> Fpath.ext (* suffix *) ->
  (Fpath.t -> 'a) -> 'a
