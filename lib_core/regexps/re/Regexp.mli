
(* Pad's easy-to-use regexp API *)
val matched : int -> string -> string

val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 :
  string -> string * string * string * string * string * string
val matched7 :
  string -> string * string * string * string * string * string * string

(* val _memo_compiled_regexp : (string, Str.regexp) Hashtbl.t *)
val split : string -> string -> string list

module Operators : sig
  val ( =~ ) : string -> string -> bool
end

type t = Re_core.t
type re = Re_core.re
