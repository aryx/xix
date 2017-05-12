open Common

type chan_kind =
  | CRed
  | CGreen
  | CBlue

  | CGrey
  | CAlpha
(* less: Cmap *)

type chan = chan_kind * int (* nb bits *)

(* less: opti: int because can fit all channels in 32 bits *)
type t = chan list (* usually 4, like R8G8B8A8 *)

(* when serializing to /dev/draw *)
type chan_serial = int
type channels_serial = int

(* deserializing *)
let kind_of_chan_serial x =
  (x lsr 4) land 15
let nbits_of_chan_serial x =
  x land 15

(* serializing *)
let int_of_chan_kind = function
  | CRed -> 0
  | CGreen -> 1
  | CBlue -> 2

  | CGrey -> 3
  | CAlpha -> 4

let mk_chan_serial (kind, nbits) = 
  let i = int_of_chan_kind kind in
  ((i land 15) lsl 4) lor (nbits land 15)

let mk_channels_serial xs = 
  raise Todo
  
let grey1 = 
  [ CGrey, 1]
let rgb16 = 
  [ CRed, 5; CGreen, 6; CBlue, 6]
let rgba32 =
  [ CRed, 8; CGreen, 8; CBlue, 8; CAlpha, 8]

(* todo: sanity checks *)
let channels_to_depth xs = 
  xs |> List.map snd |> List.fold_left (+) 0


let imgval_to_rgba img val_ =
  raise Todo

let rgba_to_imgval img rgba =
  raise Todo
