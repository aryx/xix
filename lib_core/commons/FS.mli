
val with_open_in : 
  <Cap.open_in; ..> -> (Chan.i -> 'a) -> Fpath.t -> 'a
val with_open_out : 
  <Cap.open_out; ..> -> (Chan.o -> 'a) -> Fpath.t -> 'a

val cat : <Cap.open_in; ..> -> Fpath.t -> string list

(* use Cap.open_out as removing a file is similar to erasing/overwriting
 * its content.
 *)
val remove: <Cap.open_out; ..> -> Fpath.t -> unit
