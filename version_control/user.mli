(*s: version_control/user.mli *)

(*s: type User.sign (version_control/user.mli) *)
type sign = Plus | Minus
(*e: type User.sign (version_control/user.mli) *)

(*s: type User.tz_offset (version_control/user.mli) *)
type tz_offset = {
  sign: sign;
  hours: int;
  min: int;
}
(*e: type User.tz_offset (version_control/user.mli) *)

(*s: type User.t (version_control/user.mli) *)
type t = {
  name : string;
  email: string;
  date : int64 * tz_offset;
}
(*e: type User.t (version_control/user.mli) *)

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
