(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)
open Regexp_AST
open Compile_regexp
module A = Automata

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Regexp library entry point.
 *
 * Note that this library does not depend on any C code as opposed to
 * Str so it can be used to produce pure OCaml bytecode programs.
 *
 * The regexp AST is now in Regexp.ml and the compilation part in
 * Compile_regexp.ml so what remains here is mostly the execution parts:
 *  - running a match 
 *  - handling groups
 *
 * history:
 *  - was called re.ml originally but to avoid conflict with Testo which is
 *    using internally the real ocaml-re it was renamed to Re_core.ml
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* See also Regexp_AST.t and Compile_regexp.re for types now defined in
 * separate modules that used to be defined here.
 *)

type partial_result = Full | Partial | Mismatch 

type t = Regexp_AST.t
type re = Compile_regexp.re

let print_re ch re = Automata.print_expr ch re.initial

(* ?? *)
let unknown = -2
let break = -3

(* Information used during matching *)
type info = { 
    re : Compile_regexp.re;
        (* The automata *)
    i_cols : string;
        (* Color table ([x.i_cols = x.re.cols])
           Sortcut used for performance reasons *)
    mutable positions : int array;
        (* Array of mark positions
           The mark are off by one for performance reasons *)
    mutable pos : int;
        (* Position where the match is started *)
    mutable last : int
        (* Position where the match should stop *) 
}

(* ?? *)
type substrings = (string * Automata.mark_infos * int array * int)

(*****************************************************************************)
(* Category *)
(*****************************************************************************)

let category re c =
  if c = -1 then cat_inexistant else
  (* Special category for the last newline *)
  if c = re.lnl then cat_lastnewline lor cat_newline lor cat_not_letter else
  match re.col_repr.[c] with
    'a'..'z' | 'A'..'Z' ->
      cat_letter
  | '\n' ->
      cat_not_letter lor cat_newline
  | _ ->
      cat_not_letter

(*****************************************************************************)
(* State *)
(*****************************************************************************)

let dummy_next = [||]

let unknown_state =
  { idx = unknown; real_idx = 0;
    next = dummy_next; final = [];
    desc = Automata.dummy_state }

let mk_state ncol ((idx, _, _, _, _) as desc) =
  let break_state =
    match Automata.status desc with
      A.Running -> false
    | _        -> true
  in
  { idx = if break_state then break else idx;
    real_idx = idx;
    next = if break_state then dummy_next else Array.make ncol unknown_state;
    final = [];
    desc = desc }

let find_state (re : re) (desc : A.state) : state =
  try
    Automata.States.find re.states desc
  with Not_found ->
    let st = mk_state re.ncol desc in
    Automata.States.add re.states desc st;
    st

(*****************************************************************************)
(* Match with marks *)
(*****************************************************************************)

let delta info cat c st =
  let (idx, _, _, _, _) as desc = Automata.delta info.re.tbl cat c st.desc in
  let len = Array.length info.positions in
  if idx = len && len > 0 then begin
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len
  end;
  desc

let validate info s pos st =
  let c = Char.code info.i_cols.[Char.code s.[pos]] in
  let cat = category info.re c in
  let desc' = delta info cat c st in
  let st' = find_state info.re desc' in
  st.next.(c) <- st'


let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    loop2 info s pos st st'
  else
    st

and loop2 info s pos st st' =
  let idx = st'.idx in
  if idx >= 0 then begin
    let pos = pos + 1 in
    if pos < info.last then begin
      (* It is important to place these reads before the write *)
      (* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. *)
      let st'' = st'.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
      info.positions.(idx) <- pos;
      loop2 info s pos st' st''
    end else begin
      info.positions.(idx) <- pos;
      st'
    end
  end else if idx = break then begin
    info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    validate info s pos st;
    loop info s pos st
  end

let rec loop_no_mark info s pos last st =
  if pos < last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then
      loop_no_mark info s (pos + 1) last st'
    else if idx = break then
      st'
    else begin (* Unknown *)
      validate info s pos st;
      loop_no_mark info s pos last st
    end
  else
    st

let final (info : info) (st : state) (cat : category) =
  try
    List.assq cat st.final
  with Not_found ->
    let (idx, _, _, _, _) as st' = delta info cat (-1) st in
    let res = (idx, Automata.status st') in
    st.final <- (cat, res) :: st.final;
    res

let find_initial_state re cat =
  try
    List.assq cat re.initial_states
  with Not_found ->
    let st =
      find_state re (Automata.create_state cat re.initial)
    in
    re.initial_states <- (cat, st) :: re.initial_states;
    st

let get_color re s pos =
  if pos < 0 then -1 else
  let slen = String.length s in
  if pos >= slen then -1 else
  (* Special case for the last newline *)
  if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n' then re.lnl else
  Char.code re.cols.[Char.code s.[pos]]

let rec handle_last_newline info pos st groups =
  let st' = st.next.(info.re.lnl) in
  let idx = st'.idx in
  if idx >= 0 then begin
    if groups then info.positions.(idx) <- pos + 1;
    st'
  end else if idx = break then begin
    if groups then info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    let c = info.re.lnl in
    let real_c = Char.code info.i_cols.[Char.code '\n'] in
    let cat = category info.re c in
    let desc' = delta info cat real_c st in
    let st' = find_state info.re desc' in
    st.next.(c) <- st';
    handle_last_newline info pos st groups
  end

let rec scan_str info s initial_state groups =
  let pos = info.pos in
  let last = info.last in
  if
    last = String.length s &&
    info.re.lnl <> -1 &&
    last > pos &&
    s.[last - 1] = '\n'
  then begin
    info.last <- last - 1;
    let st = scan_str info s initial_state groups in
    if st.idx = break then
      st
    else
      handle_last_newline info (last - 1) st groups
  end else if groups then
    loop info s pos initial_state
  else
    loop_no_mark info s pos last initial_state

(*****************************************************************************)
(* Matching engine *)
(*****************************************************************************)

let match_str (groups : bool) (re : re) (s : string) (pos : int) (len : int) =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re = re; i_cols = re.cols; pos = pos; last = last;
      positions =
        if groups then begin
          let n = Automata.index_count re.tbl + 1 in
          if n <= 10 then
            [|0;0;0;0;0;0;0;0;0;0|]
          else
          Array.make n 0
        end else
          [||] }
  in
  let initial_cat =
    if pos = 0 then
      cat_search_boundary lor cat_inexistant
    else
      cat_search_boundary lor category re (get_color re s (pos - 1)) in
  let initial_state = find_initial_state re initial_cat in
  let st = scan_str info s initial_state groups in
  let res =
    if st.idx = break then
      Automata.status st.desc
    else
      let final_cat =
        if last = slen then
          cat_search_boundary lor cat_inexistant
        else
          cat_search_boundary lor category re (get_color re s last) in
      let (idx, res) = final info st final_cat in
      if groups then info.positions.(idx) <- last + 1;
      res
  in
  match res with
  | A.Match m ->
      A.Match (s, m, info.positions, re.group_count)
  (* ocaml-light: can't factorize *)
(*
  | (A.Failed | A.Running) as res ->
      res
*)
  | A.Failed -> A.Failed
  | A.Running -> A.Running


(**********************************)

(*
Information about the previous character:
- does not exists
- is a letter
- is not a letter
- is a newline
- is last newline

Beginning of word:
- previous is not a letter or does not exist
- current is a letter or does not exist

End of word:
- previous is a letter or does not exist
- current is not a letter or does not exist

Beginning of line:
- previous is a newline or does not exist

Beginning of buffer:
- previous does not exist

End of buffer
- current does not exist

End of line
- current is a newline or does not exist
*)

(*
Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

(*****************************************************************************)
(* Exposing Regexp.ml builders *)
(*****************************************************************************)
(* new: this is needed now that regexp has been moved to Regexp.ml
 * to remain backward compatible with the Re API
 *)

let str = Regexp_AST.str
let char = Regexp_AST.char
let alt = Regexp_AST.alt
let seq = Regexp_AST.seq
let empty = Regexp_AST.empty
let epsilon = Regexp_AST.epsilon
let rep = Regexp_AST.rep
let rep1 = Regexp_AST.rep1
let repn = Regexp_AST.repn
let opt = Regexp_AST.opt
let bol = Regexp_AST.bol
let eol = Regexp_AST.eol
let bow = Regexp_AST.bow
let eow = Regexp_AST.eow
let bos = Regexp_AST.bos
let eos = Regexp_AST.eos
let leol = Regexp_AST.leol
let start = Regexp_AST.start
let stop = Regexp_AST.stop
let word = Regexp_AST.word
let not_boundary = Regexp_AST.not_boundary
let longest = Regexp_AST.longest
let shortest = Regexp_AST.shortest
let first = Regexp_AST.first
let greedy = Regexp_AST.greedy
let non_greedy = Regexp_AST.non_greedy
let group = Regexp_AST.group
let no_group = Regexp_AST.no_group
let nest = Regexp_AST.nest
let set = Regexp_AST.set
let rg = Regexp_AST.rg
let inter = Regexp_AST.inter
let diff = Regexp_AST.diff
let compl = Regexp_AST.compl
let any = Regexp_AST.any
let notnl = Regexp_AST.notnl
let alnum = Regexp_AST.alnum
let alpha = Regexp_AST.alpha
let ascii = Regexp_AST.ascii
let blank = Regexp_AST.blank
let cntrl = Regexp_AST.cntrl
let digit = Regexp_AST.digit
let graph = Regexp_AST.graph
let lower = Regexp_AST.lower
let print = Regexp_AST.print
let punct = Regexp_AST.punct
let space = Regexp_AST.space
let upper = Regexp_AST.upper
let xdigit = Regexp_AST.xdigit
let case = Regexp_AST.case
let no_case = Regexp_AST.no_case

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let compile = Compile_regexp.compile

let exec (*?(pos = 0) ?(len = -1)*) pos len (re : re) (s : string) : substrings =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.exec";
  match match_str true re s pos len with
    A.Match substr -> substr
  | _             -> raise Not_found

let execp (*?(pos = 0) ?(len = -1)*) pos len (re : re) (s : string) : bool =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.execp";
  match match_str false re s pos len with
    A.Match _substr -> true
  | _             -> false

let exec_partial (*?(pos = 0) ?(len = -1)*) pos len (re : re) (s : string) =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.exec_partial";
  match match_str false re s pos len with
    A.Match _ -> Full
  | A.Running -> Partial
  | A.Failed  -> Mismatch




(* substring extraction *)

let get (substrs : substrings) (i : int) : string =
  let (s, marks, pos, _) = substrs in
  if 2 * i + 1 >= Array.length marks then raise Not_found;
  let m1 = marks.(2 * i) in
  if m1 = -1 then raise Not_found;
  let p1 = pos.(m1) - 1 in
  let p2 = pos.(marks.(2 * i + 1)) - 1 in
  String.sub s p1 (p2 - p1)

let get_ofs (substrs : substrings) (i : int) : int * int =
  let (_s, marks, pos, _) = substrs in
  if 2 * i + 1 >= Array.length marks then raise Not_found;
  let m1 = marks.(2 * i) in
  if m1 = -1 then raise Not_found;
  let p1 = pos.(m1) - 1 in
  let p2 = pos.(marks.(2 * i + 1)) - 1 in
  (p1, p2)

let test (_s, marks, _pos, _) (i : int) : bool =
  if 2 * i >= Array.length marks then false else
  let idx = marks.(2 * i) in
  idx <> -1

let dummy_offset = (-1, -1)

let get_all_ofs (_s, marks, pos, count) : (int * int) array =
  let res = Array.make count dummy_offset in
  for i = 0 to Array.length marks / 2 - 1 do
    let m1 = marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = pos.(m1) in
      let p2 = pos.(marks.(2 * i + 1)) in
      res.(i) <- (p1 - 1, p2 - 1)
    end
  done;
  res

let dummy_string = ""

let get_all (s, marks, pos, count) : string array =
  let res = Array.make count dummy_string in
  for i = 0 to Array.length marks / 2 - 1 do
    let m1 = marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = pos.(m1) in
      let p2 = pos.(marks.(2 * i + 1)) in
      res.(i) <- String.sub s (p1 - 1) (p2 - p1)
    end
  done;
  res
