(*s: PATH.mli *)
(*s: signature [[PATH.find_in_path]] *)
val find_in_path :
  string (* cmd *) -> string list (* $path *) -> Common.filename
(*e: signature [[PATH.find_in_path]] *)

(*s: signature [[PATH.search_path_for_cmd]] *)
val search_path_for_cmd : string (* cmd *) -> string list (* $path *)
(*e: signature [[PATH.search_path_for_cmd]] *)
(*e: PATH.mli *)
