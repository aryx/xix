(*s: version_control/user.mli *)

(*s: type User.sign *)
type sign = Plus | Minus
(*e: type User.sign *)

(*s: type User.tz_offset *)
type tz_offset = {
  sign: sign;
  hours: int;
  min: int;
}
(*e: type User.tz_offset *)

(*s: type User.t *)
type t = {
  name : string;
  email: string;
  date : int64 * tz_offset(*option*);
}
(*e: type User.t *)

(*s: signature User.read *)
val read: IO.input -> t
(*e: signature User.read *)
(*s: signature User.write *)
val write: 'a IO.output -> t -> unit
(*e: signature User.write *)

(*s: signature User.string_of_date *)
(* for show *)
val string_of_date: (int64 * tz_offset) -> string
(*e: signature User.string_of_date *)
(*e: version_control/user.mli *)
