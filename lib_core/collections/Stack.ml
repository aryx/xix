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


type 'a t = { mutable c : 'a list }

exception Empty

let create () = { c = [] }

let clear s = s.c <- []

let push x s = s.c <- x :: s.c

let pop s =
  match s.c with
    hd::tl -> s.c <- tl; hd
  | []     -> raise Empty

let length s = List.length s.c

let iter f s = List.iter f s.c

(* addons pad *)
let top_opt s =
  match s.c with
  | [] -> None
  | x::xs -> Some x

let top s =  
  match s.c with
  | x::xs -> x
  | [] -> raise Empty

let nth i s =
  List.nth s.c i
