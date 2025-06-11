(*s: shell/Fn.ml *)
module R = Runtime

(*s: function [[Fn.flook]] *)
let flook s =
  try 
    Some (Hashtbl.find R.fns s)
  with Not_found -> None
(*e: function [[Fn.flook]] *)
(*e: shell/Fn.ml *)
