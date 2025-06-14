(*s: shell/Var.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

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
let init (caps : < Cap.env; .. >) =
  Logs.info (fun m -> m "load globals from the environment");
  let xs = Env.read_environment caps in
  xs |> List.iter (fun (k, vs) ->
    match k, vs with
    | "PATH", [x] ->
       Logs.info (fun m -> m "adjust $PATH to $path");
       let vs = Regexp_.split "[:]" x in
       let k = "path" in
       setvar k vs
    | _ -> setvar k vs
  );
(*e: function [[Var.vinit]] *)
(*e: shell/Var.ml *)
