(* Copyright 2015, 2016, 2017 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Core types *)
(*****************************************************************************)

type byte = char

(* builtin since OCaml 4.02 (bytes are mutable strings) *)
(* type bytes = string *)

type filename = string
type dirname = string

type ('a, 'b) either = Left of 'a | Right of 'b

exception Todo
exception Impossible of string

(*****************************************************************************)
(* Eq *)
(*****************************************************************************)

(*****************************************************************************)
(* Ord *)
(*****************************************************************************)

type compare = Equal | Inf | Sup

let (<=>) a b = 
  if a = b 
  then Equal 
  else 
    if a < b 
    then Inf 
    else Sup

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* let (|>) o f = f o
   builtin since OCaml 4.01 (builtin and optimized) 
*)

let spf = Printf.sprintf

let rec rnd x v =
  if x mod v = 0
  then x
  else rnd (x+1) v

(* old: used to be called do_option, or just opt *)
let if_some f = function
  | None -> ()
  | Some x -> f x

let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

let optionize f =
  try Some (f ()) with Not_found -> None


let memoized ?(use_cache=true) h k f =
  if not use_cache
  then f ()
  else
    try Hashtbl.find h k
    with Not_found ->
      let v = f () in
      begin
        Hashtbl.add h k v;
        v
      end


(* tail recursive efficient version *)
let cat file =
  let chan = open_in file in
  let rec cat_aux acc ()  =
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () |> List.rev |> (fun x -> close_in chan; x)

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

module Regexp_ = struct

let (matched: int -> string -> string) = fun i s ->
  Str.matched_group i s

let matched1 = fun s -> matched 1 s
let matched2 = fun s -> (matched 1 s, matched 2 s)
let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
let matched7 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s, matched 7 s)

let _memo_compiled_regexp = Hashtbl.create 101
let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let split sep s = Str.split (Str.regexp sep) s

end

let (=~) s re =
  Regexp_.candidate_match_func s re

(*****************************************************************************)
(* List *)
(*****************************************************************************)

module List_ = struct

let exclude p xs =
  xs |> List.filter (fun x -> not (p x))

let take n xs =
  let rec next n xs acc =
    match (n,xs) with
    | (0,_) -> List.rev acc
    | (_,[]) -> failwith "Common.take: not enough"
    | (n,x::xs) -> next (n-1) xs (x::acc) in
  next n xs []

let rec take_safe n xs =
  match (n,xs) with
  | (0,_) -> []
  | (_,[]) -> []
  | (n,x::xs) -> x::take_safe (n-1) xs

end

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

let push a aref =
  aref := a::!aref

module Stack_ = struct

(* not in ocaml 1.07 but came later 
let top s =  
  match s.c with
  | [] -> raise Empty
  | x::xs -> x
*)

let top_opt s =
  try 
    Some (Stack.top s)
  with Stack.Empty -> None

(* If have access to internal implementation of a stack:
let nth i s =
  List.nth s.c i
*)
exception Found
let nth i st =
  if i < 0 
  then raise (Invalid_argument "Stack_.nth");
  let res = ref None in
  let cnt = ref 0 in
  (try 
    st |> Stack.iter (fun e ->
      if i = !cnt
      then begin 
        res := Some e; 
        raise Found 
      end else incr cnt
    );
  with Found -> ()
  );
  match !res with
  | None -> failwith "Stack_.nth"
  | Some x -> x

end

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)

module Assoc = struct

let sort_by_val_highfirst xs =
  List.sort (fun (_k1,v1) (_k2,v2) -> compare v2 v1) xs
let sort_by_val_lowfirst xs =
  List.sort (fun (_k1,v1) (_k2,v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  List.sort (fun (k1,_v1) (k2,_v2) -> compare k2 k1) xs
let sort_by_key_lowfirst xs =
  List.sort (fun (k1,_v1) (k2,_v2) -> compare k1 k2) xs

let group_by f xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in
  
  xs |> List.iter (fun x ->
    let k = f x in
    Hashtbl.replace hkeys k true;
    Hashtbl.add h k x
  );
  Hashtbl.fold (fun k _ acc -> (k, Hashtbl.find_all h k)::acc) hkeys []

end

(*****************************************************************************)
(* Hashtbl *)
(*****************************************************************************)

module Hashtbl_ = struct    

let of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

let to_list h =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h []

end
