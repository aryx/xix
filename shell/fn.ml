module R = Runtime

let flook s =
  try 
    Some (Hashtbl.find R.fns s)
  with Not_found -> None

