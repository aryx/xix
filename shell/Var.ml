(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

module R = Runtime

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to manipulate rc shell variables *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let gvlook name =
  try 
    Hashtbl.find R.globals name
  with Not_found ->
    let var = { R.v = None } in
    Hashtbl.add R.globals name var;
    var
    
let vlook name =
  if !Runtime.runq <> []
  then 
    let t = Runtime.cur () in
    try 
      Hashtbl.find t.R.locals name
    with Not_found ->
      gvlook name
  else gvlook name


let setvar name v =
  let var = vlook name in
  var.R.v <- Some v


let vinit (_caps : < Cap.env; .. >) =
  Logs.err (fun m -> m "TODO: load from environment")
