(*s: shell/PATH.ml *)
open Common

module R = Runtime

(*s: function [[PATH.search_path_for_cmd]] *)
let search_path_for_cmd s =
  let nullpath = [""] in

  if s =~ "^/" ||
     (* Plan 9 device paths *)
     s =~ "^#" || 
     s =~ "\\./" ||
     s =~ "\\.\\./"
  then nullpath
  else
    let v = (Var.vlook "path").R.v in
    (match v with
    | None -> nullpath
    | Some xs -> xs
    )
(*e: function [[PATH.search_path_for_cmd]] *)
(*s: function [[PATH.find_in_path]] *)
let find_in_path _s _paths =
  raise Todo
(*e: function [[PATH.find_in_path]] *)
(*e: shell/PATH.ml *)
