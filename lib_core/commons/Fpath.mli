(* Poor's man filename library following the interface defined in
 * https://github.com/dbuenzli/fpath.
 *)

(* The type for paths. *)
type t
[@@deriving show]

(* The type for extensions. *)
type ext = string
[@@deriving show]

(* [v s] is the string [s] as a path *)
val v : string -> t

val to_string: t -> string

val add_seg : t -> string -> t

val append: t -> t -> t
