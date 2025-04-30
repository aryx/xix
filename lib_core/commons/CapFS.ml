(* capabilities-aware version of UChan.ml *)
open Fpath.Operators

let with_open_in (_caps : <Cap.open_in; ..>) file f =
  let chan : in_channel =
    open_in !!file 
  in
  let ichan : Chan.i = { ic = chan; origin = Chan.File file } in
  let finally () = close_in chan in
  Fun.protect ~finally (fun () -> f ichan)
