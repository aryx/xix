
type byte = char
type bytes = string

type filename = string

type ('a, 'b) either = Left of 'a | Right of 'b

type compare = Equal | Inf | Sup

exception Todo
exception Impossible of string

val spf : ('a, unit, string) format -> 'a
val pr : string -> unit
val pr2 : string -> unit

val with_file_out : (out_channel -> 'a) -> filename -> 'a
val with_file_in : (in_channel -> 'a) -> filename -> 'a

val rnd : int -> int -> int

val if_some : ('a -> unit) -> 'a option -> unit
val filter_some : 'a option list -> 'a list
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val optionize: (unit -> 'a) -> 'a option

val (<=>): 'a -> 'a -> compare

val sort_by_val_highfirst : ('a * 'b) list -> ('a * 'b) list
val sort_by_val_lowfirst : ('a * 'b) list -> ('a * 'b) list
val sort_by_key_highfirst : ('a * 'b) list -> ('a * 'b) list
val sort_by_key_lowfirst : ('a * 'b) list -> ('a * 'b) list

val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list

val memoized :
  ?use_cache:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

val cat : string -> string list

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
    val _memo_compiled_regexp : (string, Str.regexp) Hashtbl.t
    val candidate_match_func : string -> string -> bool
    val split : string -> string -> string list
  end
val ( =~ ) : string -> string -> bool

module List_ :
  sig
    val exclude : ('a -> bool) -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val take_safe : int -> 'a list -> 'a list
  end

val push : 'a -> 'a list ref -> unit

module Stack_ :
  sig
    val top_opt: 'a Stack.t -> 'a option
    val nth: int -> 'a Stack.t -> 'a
  end

module Hashtbl_ :
  sig
    val of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
  end

module Obj_ : 
  sig 
    val dump2 : Obj.t -> string 
  end

val dump : 'a -> string
val pr2_gen : 'a -> unit
