(* Copyright 2015, 2016, 2017 Yoann Padioleau, see copyright.txt *)

type byte = char

type bytes = string
(* builtin since OCaml 4.02 (bytes are mutable strings) *)

type filename = string

type ('a, 'b) either = Left of 'a | Right of 'b

(* let (|>) o f = f o
   builtin since OCaml 4.01 (builtin and optimized) 
*)

let spf = Printf.sprintf

let pr s =
  print_string s;
  print_string "\n";
  flush stdout

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr


let with_file_out f file = 
  let chan = open_out file in
  let res = f chan in
  close_out chan;
  res

(* less: unwind_protect? *)
let with_file_in f file = 
  let chan = open_in file in
  let res = f chan in
  close_in chan;
  res

let push a aref =
  aref := a::!aref

exception Todo
exception Impossible of string

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

let map_filter f xs = xs |> List.map f |> filter_some


let sort_by_val_highfirst xs =
  List.sort (fun (k1,v1) (k2,v2) -> compare v2 v1) xs
let sort_by_val_lowfirst xs =
  List.sort (fun (k1,v1) (k2,v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  List.sort (fun (k1,v1) (k2,v2) -> compare k2 k1) xs
let sort_by_key_lowfirst xs =
  List.sort (fun (k1,v1) (k2,v2) -> compare k1 k2) xs

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


module Hashtbl_ = struct    

let of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

let to_list h =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h []
end



module Obj_ = struct
(* start of dumper.ml *)

(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * dumper.ml 1.2 2005/02/06 12:38:21 rich Exp
 *)
open Printf
open Obj

let rec dump2 r =
  if is_int r then
    string_of_int (magic r : int)
  else (				(* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (field r n :: acc) n
    in
    let rec is_list r =
      if is_int r then (
        if (magic r : int) = 0 then true (* [] *)
        else false
      ) else (
        let s = size r and t = tag r in
        if t = 0 && s = 2 then is_list (field r 1) (* h :: t *)
        else false
      )
    in
    let rec get_list r =
      if is_int r then []
      else let h = field r 0 and t = get_list (field r 1) in h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible in
       * pure OCaml at the moment.
       *)
      "<" ^ name ^ ">"
    in

    let s = size r and t = tag r in

    (* From the tag, determine the type of block. *)
    if is_list r then ( (* List. *)
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump2 fields) ^ "]"
    )
    else if t = 0 then (		(* Tuple, array, record. *)
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )

    (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
     * clear if very large constructed values could have the same
     * tag. XXX *)
    else if t = lazy_tag then opaque "lazy"
    else if t = closure_tag then opaque "closure"
    else if t = object_tag then (	(* Object. *)
      let fields = get_fields [] s in
      let clasz, id, slots =
        match fields with h::h'::t -> h, h', t | _ -> assert false in
      (* No information on decoding the class (first field).  So just print
       * out the ID and the slots.
       *)
      "Object #" ^ dump2 id ^
        " (" ^ String.concat ", " (List.map dump2 slots) ^ ")"
    )
    else if t = infix_tag then opaque "infix"
    else if t = forward_tag then opaque "forward"

    else if t < no_scan_tag then (	(* Constructed value. *)
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
        " (" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )
    else if t = string_tag then (
      "\"" ^ String.escaped (magic r : string) ^ "\""
    )
    else if t = double_tag then (
      string_of_float (magic r : float)
    )
    else if t = abstract_tag then opaque "abstract"
    else if t = custom_tag then opaque "custom"
    else if t = final_tag then opaque "final"
    else failwith ("dump: impossible tag (" ^ string_of_int t ^ ")")
  )

end

let dump v = Obj_.dump2 (Obj.repr v)

(* end of dumper.ml *)

(*
let (dump : 'a -> string) = fun x ->
  Dumper.dump x
*)
let pr2_gen x = pr2 (dump x)
