open Fpath_.Operators
open Chan (* for fields *)

let with_open_in (f : Chan.i -> 'a) (file : Fpath.t) : 'a =
  let chan : in_channel =
    (* nosemgrep: do-not-use-open-in *)
    open_in !!file 
  in
  let ichan : Chan.i = { ic = chan; origin = Chan.File file } in
  Fun.protect ~finally:(fun () -> close_in chan) (fun () -> f ichan)

let with_open_out (f : Chan.o -> 'a) (file : Fpath.t) : 'a =
  let chan : out_channel =
    (* nosemgrep: use-caps *)
    open_out !!file 
  in
  let ochan : Chan.o = { oc = chan; dest = Chan.OutFile file } in
  Fun.protect ~finally:(fun () -> close_out chan) (fun () -> f ochan)
