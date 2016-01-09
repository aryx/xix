(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)

type filename = string

let spf = Printf.sprintf

type ('a, 'b) either = Left of 'a | Right of 'b

let with_file_out f file = 
  let chan = open_out file in
  let res = f chan in
  close_out chan;
  res

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

(* used to be called do_option, or opt *)
let if_some f = function
  | None -> ()
  | Some x -> f x

let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

let map_filter f xs = xs |> List.map f |> filter_some

let exclude p xs =
  xs |> List.filter (fun x -> not (p x))

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

let (=~) s re =
  candidate_match_func s re
