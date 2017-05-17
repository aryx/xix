
let (windows: (Window.wid, Window.t) Hashtbl.t) = Hashtbl.create 11

(* rio(1) uses the term 'current' *)
let (current: Window.t option ref)  = ref None

(* a bit like cpu(), up() in the kernel, a convenient global *)
let win () =
  !current

let debug = ref false
