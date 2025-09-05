(*****************************************************************************)
(* Core types and exceptions *)
(*****************************************************************************)

type byte = char
(* type bytes = string *)

(* TODO: delete, use Fpath.t *)
type filename = string

exception Todo
exception Impossible of string

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

val spf : ('a, unit, string) format -> 'a

(*****************************************************************************)
(* Option *)
(*****************************************************************************)
val ( ||| ) : 'a option -> 'a -> 'a

(*****************************************************************************)
(* Result *)
(*****************************************************************************)

(*****************************************************************************)
(* Basic types *)
(*****************************************************************************)

module Int_ :
  sig
    val rnd: int -> int -> int
  end

(*****************************************************************************)
(* Regexps *)
(*****************************************************************************)

val ( =~ ) : string -> string -> bool
module Regexp_ :
  sig
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
  end

(*****************************************************************************)
(* Collections *)
(*****************************************************************************)

(* See also Set_.ml and Map_.ml in collections/ *)

module List_ :
  sig
    val exclude : ('a -> bool) -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val take_safe : int -> 'a list -> 'a list
    (* was in common2.ml *)
    val join_gen: 'a -> 'a list -> 'a list
    val list_of_string: string -> char list
    val span: ('a -> bool) -> 'a list -> 'a list * 'a list
    val index_list_1: 'a list -> ('a * int) list
    val zip: 'a list -> 'b list -> ('a * 'b) list
  end


module Stack_ :
  sig
    val push : 'a -> 'a list ref -> unit

    val nth: int -> 'a Stack.t -> 'a
  end

module Assoc :
  sig
    val sort_by_val_highfirst : ('a * 'b) list -> ('a * 'b) list
    val sort_by_val_lowfirst : ('a * 'b) list -> ('a * 'b) list
    val sort_by_key_highfirst : ('a * 'b) list -> ('a * 'b) list
    val sort_by_key_lowfirst : ('a * 'b) list -> ('a * 'b) list

    val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
  end

module Hashtbl_ :
  sig
    val of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

    type 'a set = ('a, bool) Hashtbl.t

    val memoized :
      ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b
  end
