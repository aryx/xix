(*****************************************************************************)
(* Core types and exceptions *)
(*****************************************************************************)

type byte = char

exception Todo
exception Impossible of string

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

(* shorter than Printf.sprintf *)
val spf : ('a, unit, string) format -> 'a

module Fmt_ : 
  sig
    val with_buffer_to_string: (Format.formatter -> unit) -> string
  end

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)
module Fun_ :
  sig
    val once: 'a option ref -> (unit -> 'a) -> 'a
  end

(*****************************************************************************)
(* Basic types *)
(*****************************************************************************)

(* For lib_core/system/plan9.ml and kernel/ *)
type int8 = int
type int16 = int
type int32 = int
type int64 = int

module Int_ :
  sig
    val rnd: int (* n *) -> int (* rnd_ *) -> int
    val maxround: int (* max *) -> int (* n *) -> int (* rnd_ *) -> int

    val log2: int -> int
    val roundup: int -> int (* power of 2 *) -> int
  end

module String_ :
  sig
    (* if the string is longer than max_len then it will be abbreviated
     * as "<str ... (<n> bytes)"
     *)
    val show_max: int (* max_len *) -> string -> string
    (* those functions do not raise exn; if the integer is higher than the
     * length of the string, the empty string is returned.
     *)
    val drop_prefix: int -> string -> string
    val drop_suffix: int -> string -> string

    val of_chars : char list -> string
  end

(*****************************************************************************)
(* Regexps *)
(*****************************************************************************)

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

    module Operators : sig
      val ( =~ ) : string -> string -> bool
    end
  end

(*****************************************************************************)
(* Containers *)
(*****************************************************************************)
val ( ||| ) : 'a option -> 'a -> 'a

module Either_ : 
  sig
    (* just for deriving show *)
    type ('a, 'b) t = ('a, 'b) Either.t
    [@@deriving show]
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
    val enum : int -> int -> int list
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
    (* just for deriving show *)
    type ('k, 'v) t = ('k, 'v) Hashtbl.t
    [@@deriving show]

    val create : unit -> ('a, 'b) Hashtbl.t
    val of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

    type 'a set = ('a, bool) Hashtbl.t

    val hashset_of_list : 'a list -> 'a set
    val hashset_to_list : 'a set -> 'a list

    val memoized :
      ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b
  end

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
module Unix_ : 
  sig
    (* just for deriving show *)
    type file_descr = Unix.file_descr
    [@@deriving show]
  end

module Lexing_ : 
  sig
    (* just for deriving show *)
    type lexbuf = Lexing.lexbuf
    [@@deriving show]
  end

module Out_channel_ : 
  sig
    (* just for deriving show *)
    type t = out_channel
    [@@deriving show]
  end
