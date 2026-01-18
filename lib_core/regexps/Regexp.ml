open Common

(* one line change :) *)
module Str = Re_str

type t = Re_core.t
type re = Re_core.re

let matched (i : int) (s : string) : string =
  Str.matched_group i s

let matched1 s = matched 1 s
let matched2 s = (matched 1 s, matched 2 s)
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)
let matched4 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
let matched5 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
let matched6 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
let matched7 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s, matched 7 s)

let _memo_compiled_regexp = Hashtbl.create 101
let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    Hashtbl_.memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let split sep s = Str.split (Str.regexp sep) s

  module Operators = struct
    let (=~) s re =
      candidate_match_func s re
  end
