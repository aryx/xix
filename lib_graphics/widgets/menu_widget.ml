open Common

(* inspiration is GToolbox in lablgtk *)
type item = (string * (unit -> unit))
type items = item list

let menu items (m, mouse) (display, desktop, view) =
  raise Todo
