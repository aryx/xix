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

(* $Id: set.mli,v 1.11 1997/12/09 09:11:48 xleroy Exp $ *)

(* Module [Set]: sets over ordered types *)

(* This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance. *)


type 'elt t
      (* The type of sets. *)
val empty: 'elt t
      (* The empty set. *)
val is_empty: 'elt t -> bool
    (* Test whether a set is empty or not. *)
val mem: 'elt ->'elt t -> bool
    (* [mem x s] tests whether [x] belongs to the set [s]. *)
val add: 'elt ->'elt t -> 'elt t
    (* [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
val remove: 'elt ->'elt t -> 'elt t
    (* [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)
val union:'elt t ->'elt t -> 'elt t
val inter:'elt t ->'elt t -> 'elt t
val diff:'elt t ->'elt t -> 'elt t
    (* Union, intersection and set difference. *)
val compare:'elt t ->'elt t -> int
    (* Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)
val equal:'elt t ->'elt t -> bool
    (* [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain the same elements. *)
val subset:'elt t ->'elt t -> bool
    (* [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)
val iter: ('elt -> unit) ->'elt t -> unit
    (* [iter f s] applies [f] in turn to all elements of [s].
       The order in which the elements of [s] are presented to [f]
       is unspecified. *)
val fold: ('elt -> 'a -> 'a) -> 'elt t -> 'a -> 'a
    (* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s].
       The order in which elements of [s] are presented to [f] is
       unspecified. *)
val cardinal:'elt t -> int
    (* Return the number of elements of a set. *)
val elements:'elt t -> 'elt list
    (* Return the list of all elements of the given set.
       The elements appear in the list in some unspecified order. *)
val choose:'elt t -> 'elt
    (* Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

val singleton: 'elt -> 'elt t
(** [singleton x] returns the one-element set containing only [x]. *)

(* addons pad *)
val of_list: 'elt list -> 'elt t
