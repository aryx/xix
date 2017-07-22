
type sign = Plus | Minus

type tz_offset = {
  sign: sign;
  hours: int;
  min: int;
}

type t = {
  name : string;
  email: string;
  date : int64 * tz_offset;
}

val read: IO.input -> t
val write: 'a IO.output -> t -> unit
