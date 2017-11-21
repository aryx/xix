(*s: version_control/user.mli *)

(*s: type User.tz_offset *)
type timezone_offset = int (* +/- hours, [-12, +12] *)
(*e: type User.tz_offset *)

(*s: type User.t *)
type t = {
  name : string;
  email: string;
  date : int64 (* seconds *) * timezone_offset;
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
val string_of_date: (int64 * timezone_offset) -> string
(*e: signature User.string_of_date *)
(*e: version_control/user.mli *)
