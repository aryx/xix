(* Copyright 2015, 2016, 2017, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Core types *)
(*****************************************************************************)

(* builtin since OCaml 4.02 (bytes are mutable strings) *)
(* type bytes = string *)

type byte = char

exception Todo
exception Impossible of string

(*****************************************************************************)
(* Option *)
(*****************************************************************************)

(* not sure why but can't use let (?:) a b = ... then at use time ocaml yells*)
let ( ||| ) a b =
  match a with
  | Some x -> x
  | None -> b

(* TODO: let* once ocaml-light supports it *)

(*****************************************************************************)
(* Result *)
(*****************************************************************************)
(* TODO: let/ once ocaml-light supports it *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* let (|>) o f = f o
   builtin since OCaml 4.01 (builtin and optimized) 
*)

let spf = Printf.sprintf

(* should be in Hashtbl_ below but it's also used by Regexp_ module
 * so has to be defined before
 *)
let memoized h k f =
    try Hashtbl.find h k
    with Not_found ->
      let v = f () in
      begin
        Hashtbl.add h k v;
        v
      end

(*****************************************************************************)
(* Basic types *)
(*****************************************************************************)

module Int_ = struct

let rec rnd x v =
  if x mod v = 0
  then x
  else rnd (x+1) v

end

module String_ = struct

(* also in Base/Core
 * alt: drop_left/drop_right, and could raise exn
 *)
let drop_prefix n s =
  let len = String.length s in
  if n >= len then ""
  else String.sub s n (len - n)

let drop_suffix n s =
  let len = String.length s in
  if n >= len then ""
  else String.sub s 0 (len - n)

end

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

  module Operators = struct
    let (=~) s re =
      candidate_match_func s re
  end

end


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

let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let enum x n =
  if not(x <= n)
  then failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n::acc else enum_aux (x::acc) (x+1) n
  in
  List.rev (enum_aux [] x n)

let (list_of_string: string -> char list) = function
    "" -> []
  | s -> (enum 0 ((String.length s) - 1) |> List.map (String.get s))


let rec zip xs ys =
  match (xs,ys) with
  | ([],[]) -> []
  | ([],_) -> failwith "zip: not same length"
  | (_,[]) -> failwith "zip: not same length"
  | (x::xs,y::ys) -> (x,y)::zip xs ys

let index_list xs =
  if xs = [] 
  then [] (* enum 0 (-1) generate an exception *)
  else zip xs (enum 0 ((List.length xs) -1))

let index_list_1 xs =
  xs |> index_list |> List.map (fun (x,i) -> x, i+1)


let rec (span: ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p -> function
  | []    -> ([], [])
  | x::xs ->
      if p x then
        let (l1, l2) = span p xs in
        (x::l1, l2)
      else ([], x::xs)

end

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)


module Stack_ = struct

let push a aref =
  aref := a::!aref

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

(* alt: could be a further nested module Hashtbl_.Set.t *)
type 'a set = ('a, bool) Hashtbl.t

let memoized = memoized

let hashset_of_list (xs : 'a list) : 'a set =
  let h = Hashtbl.create (List.length xs) in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  h

let hashset_to_list h = to_list h |> List.map fst

end
