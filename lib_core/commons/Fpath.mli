(* Poor's man filename library following the interface defined in
 * https://github.com/dbuenzli/fpath.
 *)

(* The type for paths. *)
type t
[@@deriving show]

(* [v s] is the string [s] as a path *)
val v : string -> t

val to_string: t -> string

module Operators : sig
  (* = add_seg *)
  (*val ( / ) : Fpath.t -> string -> Fpath.t *)

  (* = append *)
  (* val ( // ) : Fpath.t -> Fpath.t -> Fpath.t *)

  (* = to_string *)
  val ( !! ) : t -> string
end
