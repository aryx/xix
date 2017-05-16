
let (windows: (Window.wid, Window.t) Hashtbl.t) = Hashtbl.create 11

(* less: rename winput? or just input? *)
let (current: Window.t option ref)  = ref None
