(* 
 * OCaml hacks to support reflection (works with ocamltarzan).
 *
 * See also sexp.ml, json.ml, and xml.ml for other "reflective" techniques.
 *)

(* OCaml values (a restricted form of expressions) *)
type v = 
  | VUnit 
  | VBool of bool | VFloat of float | VInt of int
  | VChar of char | VString of string

  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list

  | VVar of (string * int)
  | VArrow of string

  (* special cases *) 
  | VNone | VSome of v
  | VList of v list
  | VRef of v

  | VTODO of string

(* building blocks, used by code generated using ocamltarzan *)
val vof_unit   : unit -> v
val vof_bool   : bool -> v
val vof_int    : int -> v
val vof_float   : float -> v
val vof_string : string -> v
val vof_list   : ('a -> v) -> 'a list -> v
val vof_option : ('a -> v) -> 'a option -> v
val vof_ref    : ('a -> v) -> 'a ref -> v
val vof_either    : ('a -> v) -> ('b -> v) -> ('a, 'b) Either.t -> v

(* regular pretty printer (not via sexp, but using Format) *)
val string_of_v: v -> string
