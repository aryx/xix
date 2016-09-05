open Common

module R = Runtime

let gvlook name =
  try 
    Hashtbl.find R.globals name
  with Not_found ->
    let var = { R.v = None } in
    Hashtbl.add R.globals name var;
    var
    

(* less: could pass thread in param? *)
let vlook name =
  if !Runtime.runq <> []
  then raise Todo
  else raise Todo

let setvar name v =
  let var = vlook name in
  var.R.v <- Some v



let vinit () =
  pr2 "TODO: load from environment"

