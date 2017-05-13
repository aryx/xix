open Common

type chan_kind =
  | CRed
  | CGreen
  | CBlue

  | CAlpha

  | CGrey
  | CMap

type chan = chan_kind * int (* nb bits *)

(* less: opti: int because can fit all channels in 32 bits *)
type t = chan list (* usually 4, like R8G8B8A8 *)

let grey1 =
  [ CGrey, 1]
let rgb16 =
  [ CRed, 5; CGreen, 6; CBlue, 6]
let rgb24 =
  [ CRed, 8; CGreen, 8; CBlue, 8]
let rgba32 =
  [ CRed, 8; CGreen, 8; CBlue, 8; CAlpha, 8]
let cmap8 =
  [ CMap, 8]


(* todo: sanity checks *)
let depth_of_channels xs = 
  xs |> List.map snd |> List.fold_left (+) 0



(* when serializing to /dev/draw *)
type chan_serial = int
type channels_serial = int

(* when reading from /dev/draw/new *)
type channels_str = string

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
  | CMap -> 5

let mk_chan_serial (kind, nbits) = 
  let i = int_of_chan_kind kind in
  ((i land 15) lsl 4) lor (nbits land 15)

let mk_channels_serial xs =
  match xs with
  | [x] -> mk_chan_serial x
  | [x1; x2; x3] -> 
    ((mk_chan_serial x1) lsl 16) lor
    ((mk_chan_serial x2) lsl 8) lor
    (mk_chan_serial x3)
  | [x1; x2; x3; x4] -> 
    ((mk_chan_serial x1) lsl 24) lor
    ((mk_chan_serial x2) lsl 16) lor
    ((mk_chan_serial x3) lsl 8) lor
    (mk_chan_serial x4)
  | _ -> raise Todo


let channels_of_str str =
  match str with
  | "m8" -> cmap8
  | _ -> failwith (spf "channels format not supported yet: %s" str)
  


(* do not pass an img here otherwise mutually dependent modules *)
let imgval_to_rgba chan val_ =
  raise Todo

let rgba_to_imgval chan rgba =
  raise Todo
