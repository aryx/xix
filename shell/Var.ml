(*s: shell/Var.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

module R = Runtime

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to manipulate rc shell variables *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(*s: function [[Var.gvlook]] *)
let gvlook (name : R.varname) : R.var =
  try 
    Hashtbl.find R.globals name
  with Not_found ->
    let var = { R.v = None } in
    Hashtbl.add R.globals name var;
    var
(*e: function [[Var.gvlook]] *)
(*s: function [[Var.vlook]] *)
let vlook name =
  if !Runtime.runq <> []
  then 
    let t = Runtime.cur () in
    try 
      Hashtbl.find t.R.locals name
    with Not_found ->
      gvlook name
  else gvlook name
(*e: function [[Var.vlook]] *)

(*s: function [[Var.setvar]] *)
let setvar (name : Runtime.varname) (v : Runtime.value) : unit =
  let var = vlook name in
  var.R.v <- Some v
(*e: function [[Var.setvar]] *)

(*s: function [[Var.vinit]] *)
let vinit (_caps : < Cap.env; .. >) =
  Logs.err (fun m -> m "TODO: load from environment")
(*e: function [[Var.vinit]] *)
(*e: shell/Var.ml *)
