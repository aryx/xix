(*s: Var.mli *)
(* Helpers to manipulate rc shell variables *)

(*s: signature [[Var.gvlook]] *)
(* This will only look for globals (in Runtime.globals) *)
val gvlook : Runtime.varname -> Runtime.var
(*e: signature [[Var.gvlook]] *)
(*s: signature [[Var.vlook]] *)
(* This will look first in the locals (in Runtime.cur().locals) and then
 * in globals (in Runtime.globals)
 *)
val vlook : Runtime.varname -> Runtime.var
(*e: signature [[Var.vlook]] *)
(*s: signature [[Var.setvar]] *)
(* Override the content of a (local or global) variable *)
val setvar : Runtime.varname -> Runtime.value -> unit
(*e: signature [[Var.setvar]] *)
(*s: signature [[Var.vinit]] *)
(* Populate Runtime.globals from the current environment *)
val vinit : < Cap.env ; .. > -> unit
(*e: signature [[Var.vinit]] *)
(*e: Var.mli *)
