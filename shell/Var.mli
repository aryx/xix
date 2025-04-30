(* Helpers to manipulate rc shell variables *)

(* This will only look for globals (in Runtime.globals) *)
val gvlook : Runtime.varname -> Runtime.var

(* This will look first in the locals (in Runtime.cur().locals) and then
 * in globals (in Runtime.globals)
 *)
val vlook : Runtime.varname -> Runtime.var

(* Override the content of a (local or global) variable *)
val setvar : Runtime.varname -> Runtime.value -> unit

(* Populate Runtime.globals from the current environment *)
val vinit : < Cap.env ; .. > -> unit
