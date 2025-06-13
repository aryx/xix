(*s: PATH.mli *)
(*s: signature [[PATH.find_in_path]] *)
val find_dir_with_cmd_in_PATH :
  string (* cmd *) -> Fpath.t list (* $path *) -> Fpath.t
(*e: signature [[PATH.find_in_path]] *)

(*s: signature [[PATH.search_path_for_cmd]] *)
(* Returns None when there is no need to use $path because the cmd is
 * already using an absolute, relative, or special path.
 *)
val var_PATH_for_cmd_opt : string (* cmd *) -> 
  Fpath.t list (* content of $path *) option
(*e: signature [[PATH.search_path_for_cmd]] *)
(*e: PATH.mli *)
