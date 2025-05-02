(* returns None if file does not exist or can't be stat'ed for some reasons *)
val timeof : Fpath.t -> float option

val str_of_time : float option -> string
