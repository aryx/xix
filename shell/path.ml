open Common

module R = Runtime

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


let find_in_path s paths =
  raise Todo
