
val with_open_in : 
  (Chan.i -> 'a) -> Fpath.t -> 'a

val with_open_out : 
  (Chan.o -> 'a) -> Fpath.t -> 'a
