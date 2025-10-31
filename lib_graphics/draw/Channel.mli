
type chan_kind =
  | CRed
  | CGreen
  | CBlue

  | CAlpha

  | CGrey
  | CMap

type chan = chan_kind * int (* nb bits *)

type t = chan list (* usually 4, like R8G8B8A8 *)

val rgb24: t
val rgba32: t
val grey1: t
val grey8: t

(* when serializing to /dev/draw *)
type chan_serial = int
type channels_serial = int

(* when reading from /dev/draw/new *)
type channels_str = string

val mk_channels_serial: t -> channels_serial

val channels_of_str: channels_str -> t

val depth_of_channels: t -> int

