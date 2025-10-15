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

(* $Id: list.mli,v 1.11 1997/10/31 12:59:25 doligez Exp $ *)

(* Module [List]: list operations *)

val length : 'a list -> int
        (* Return the length (number of elements) of the given list. *)
val hd : 'a list -> 'a
        (* Return the first element of the given list. Raise
           [Failure "hd"] if the list is empty. *)
val tl : 'a list -> 'a list
        (* Return the given list without its first element. Raise
           [Failure "tl"] if the list is empty. *)
val nth : 'a list -> int -> 'a
        (* Return the n-th element of the given list.
           The first element (head of the list) is at position 0.
           Raise [Failure "nth"] if the list is too short. *)
val rev : 'a list -> 'a list
        (* List reversal. *)
val concat  : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
        (* Catenate (flatten) a list of lists. *)

(** Iterators *)

val iter : ('a -> unit) -> 'a list -> unit
        (* [List.iter f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an]. It is equivalent to
           [begin f a1; f a2; ...; f an; () end]. *)
val map : ('a -> 'b) -> 'a list -> 'b list
        (* [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the list [[f a1; ...; f an]]
           with the results returned by [f]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        (* [List.fold_left f a [b1; ...; bn]] is
           [f (... (f (f a b1) b2) ...) bn]. *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        (* [List.fold_right f [a1; ...; an] b] is
           [f a1 (f a2 (... (f an b) ...))]. *)

(** Iterators on two lists *)

val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        (* [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [List.map2 f [a1; ...; an] [b1; ...; bn]] is
           [[f a1 b1; ...; f an bn]].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        (* [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
           [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        (* [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
           [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)

(** List scanning *)

val for_all : ('a -> bool) -> 'a list -> bool
        (* [for_all p [a1; ...; an]] checks if all elements of the list
           satisfy the predicate [p]. That is, it returns
           [(p a1) & (p a2) & ... & (p an)]. *)
val exists : ('a -> bool) -> 'a list -> bool
        (* [exists p [a1; ...; an]] checks if at least one element of
           the list satisfies the predicate [p]. That is, it returns
           [(p a1) or (p a2) or ... or (p an)]. *)
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [for_all] and [exists], but for a two-argument predicate.
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val mem : 'a -> 'a list -> bool
        (* [mem a l] is true if and only if [a] is equal
           to an element of [l]. *)
val memq : 'a -> 'a list -> bool
        (* Same as [mem], but uses physical equality instead of structural
           equality to compare list elements. *)

(** Association lists *)

val assoc : 'a -> ('a * 'b) list -> 'b
        (* [assoc a l] returns the value associated with key [a] in the list of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in list [l].
           Raise [Not_found] if there is no value associated with [a] in the
           list [l]. *)
val mem_assoc : 'a -> ('a * 'b) list -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
val assq : 'a -> ('a * 'b) list -> 'b
        (* Same as [assoc], but uses physical equality instead of structural
           equality to compare keys. *)

(** Lists of pairs *)

val split : ('a * 'b) list -> 'a list * 'b list
        (* Transform a list of pairs into a pair of lists:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])]
        *)
val combine : 'a list -> 'b list -> ('a * 'b) list
        (* Transform a pair of lists into a list of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument] if the two lists
           have different lengths. *)


(* ported from 3.12 *)
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Sort a list in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see Array.sort for
   a complete specification).  For example,
   {!Pervasives.compare} is a suitable comparison function.
   The resulting list is sorted in increasing order.
   [List.sort] is guaranteed to run in constant heap space
   (in addition to the size of the result list) and logarithmic
   stack space.

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*)

val filter : ('a -> bool) -> 'a list -> 'a list
(** [filter p l] returns all the elements of the list [l]
   that satisfy the predicate [p].  The order of the elements
   in the input list is preserved.  *)

val find_all : ('a -> bool) -> 'a list -> 'a list
(** [find_all] is another name for {!List.filter}. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [partition p l] returns a pair of lists [(l1, l2)], where
   [l1] is the list of all the elements of [l] that
   satisfy the predicate [p], and [l2] is the list of all the
   elements of [l] that do not satisfy [p].
   The order of the elements in the input list is preserved. *)

val find : ('a -> bool) -> 'a list -> 'a
(** [find p l] returns the first element of the list [l]
   that satisfies the predicate [p].
   Raise [Not_found] if there is no value that satisfies [p] in the
   list [l]. *)
