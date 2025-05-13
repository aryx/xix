open Fpath_.Operators
open Chan (* for fields *)

let with_open_in (f : Chan.i -> 'a) (file : Fpath.t) : 'a =
  let chan : in_channel =
    open_in !!file 
  in
  let ichan : Chan.i = { ic = chan; origin = Chan.File file } in
  let finally () : unit = close_in chan in
  Fun.protect ~finally (fun () -> f ichan)

let with_open_out (f : Chan.o -> 'a) (file : Fpath.t) : 'a =
  let chan : out_channel =
    open_out !!file 
  in
  let ochan : Chan.o = { oc = chan; p = file } in
  let finally () = close_out chan in
  Fun.protect ~finally (fun () -> f ochan)
