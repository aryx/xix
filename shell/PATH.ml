(*s: shell/PATH.ml *)
open Common
open Fpath_.Operators
open Regexp.Operators

module R = Runtime

(*s: function [[PATH.search_path_for_cmd]] *)
let var_PATH_for_cmd_opt (s : string) : Fpath.t list option =
  (* no need for PATH resolution when commands use absolute, relative, or
   * special paths.
   *)
  if s =~ "^/" ||
     (* Plan 9 device paths *)
     s =~ "^#" || 
     s =~ "\\./" ||
     s =~ "\\.\\./"
  then None
  else
    let v = (Var.vlook "path").R.v in
    (match v with
    | None -> Some []
    | Some xs -> Some (Fpath_.of_strings xs)
    )
(*e: function [[PATH.search_path_for_cmd]] *)
(*s: function [[PATH.find_in_path]] *)
let find_dir_with_cmd_in_PATH (cmd : string) (paths : Fpath.t list) : Fpath.t =
  paths |> List.find (fun dir ->
    let res = Sys.file_exists !!(dir / cmd) in
    if res
    then Logs.info (fun m -> m "found %s in %s" cmd !!dir);
    res
  )

(*e: function [[PATH.find_in_path]] *)
(*e: shell/PATH.ml *)
