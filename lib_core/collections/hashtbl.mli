(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: hashtbl.mli,v 1.13 1997/10/31 12:59:23 doligez Exp $ *)

(* Module [Hashtbl]: hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type ('a, 'b) t
        (* The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a,'b) t
        (* [Hashtbl.create n] creates a new, empty hash table, with
           initial size [n].  The table grows as needed, so [n] is
           just an initial guess.  Better results are said to be
           achieved when [n] is a prime number.
           Raise [Invalid_argument "hashtbl__new"] if [n] is less than 1. *)

val clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
        (* [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [remove tbl x], the previous
           binding for [x], if any, is restored.
           (This is the semantics of association lists.) *)

val find : ('a, 'b) t -> 'a -> 'b
        (* [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
        (* [Hashtbl.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val remove : ('a, 'b) t -> 'a -> unit
        (* [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        (* [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)


(*** The polymorphic hash primitive *)

val hash : 'a -> int
        (* [Hashtbl.hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)

external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
        (* [Hashtbl.hash_param n m x] computes a hash value for [x], with the
           same properties as for [hash]. The two extra parameters [n] and
           [m] give more precise control over hashing. Hashing performs a
           depth-first, right-to-left traversal of the structure [x], stopping
           after [n] meaningful nodes were encountered, or [m] nodes,
           meaningful or not, were encountered. Meaningful nodes are: integers;
           floating-point numbers; strings; characters; booleans; and constant
           constructors. Larger values of [m] and [n] means that more
           nodes are taken into account to compute the final hash
           value, and therefore collisions are less likely to happen.
           However, hashing takes longer. The parameters [m] and [n]
           govern the tradeoff between accuracy and speed. *)

(* ported from 3.12 *)
val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
   followed by {!Hashtbl.add}[ tbl x y]. *)
