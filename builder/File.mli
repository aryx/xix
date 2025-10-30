(*s: File.mli *)
(*s: signature [[File.timeof]] *)
(* returns None if file does not exist or can't be stat'ed for some reasons *)
val timeof : Fpath.t -> float option
(*e: signature [[File.timeof]] *)

(*s: signature [[File.str_of_time]] *)
val str_of_time : float option -> string
(*e: signature [[File.str_of_time]] *)
(*e: File.mli *)
