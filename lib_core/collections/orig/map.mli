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

(* $Id: map.mli,v 1.11 1997/12/09 09:11:47 xleroy Exp $ *)

(* Module [Map]: association tables over ordered types *)

(* This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

type ('key, 'a) t
      (* The type of maps from type [key] to type ['a]. *)
val empty: ('key, 'a) t
      (* The empty map. *)
val add: 'key -> 'a -> ('key, 'a) t -> ('key, 'a) t
    (* [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)
val find: 'key -> ('key, 'a) t -> 'a
    (* [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
val remove: 'key -> ('key, 'a) t -> ('key, 'a) t
    (* [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)
val iter: ('key -> 'a -> unit) -> ('key, 'a) t -> unit
    (* [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument. The order in which the bindings are passed to
       [f] is unspecified. Only current bindings are presented to [f]:
       bindings hidden by more recent bindings are not passed to [f]. *)
val map: ('a -> 'b) -> ('key, 'a) t -> ('key, 'b) t
    (* [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The order in which the associated values are passed to [f]
       is unspecified. *)
val fold: ('key -> 'a -> 'b -> 'b) -> ('key, 'a) t -> 'b -> 'b
    (* [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m],
       and [d1 ... dN] are the associated data.
       The order in which the bindings are presented to [f] is
       unspecified. *)

val mem: 'key -> ('key, 'a) t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

(* addons pad *)
val to_list: ('key, 'a) t -> ('key * 'a) list
