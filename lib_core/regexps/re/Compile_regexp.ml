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
open Regexp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type state = { 
    idx : int;
        (* Index of the current position in the position table.
           Not yet computed transitions point to a dummy state where
           [idx] is set to [unknown];
           If [idx] is set to [break] for states that either always
           succeed or always fail. *)
    real_idx : int;
        (* The real index, in case [idx] is set to [break] *)
    next : state array;
        (* Transition table, indexed by color *)
    mutable final :
      (Automata.category * (Automata.idx * Automata.status)) list;
        (* Mapping from the category of the next character to
           - the index where the next position should be saved
           - possibly, the list of marks (and the corresponding indices)
             corresponding to the best match *)
    desc : Automata.state
        (* Description of this state of the automata *) 
}

(* Automata (compiled regular expression) *)
type re = {
    initial : Automata.expr;
        (* The whole regular expression *)
    mutable initial_states : (int * state) list;
        (* Initial states, indexed by initial category *)
    cols : string;
        (* Color table *)
    col_repr : string;
        (* Table from colors to one character of this color *)
    ncol : int;
        (* Number of colors *)
    lnl : int;
        (* Color of the last newline *)
    mutable tbl : Automata.working_area;
        (* Temporary table used to compute the first available index
           when computing a new state *)
    states : state Automata.States.t;
        (* States of the deterministic automata *)
    group_count : int
        (* Number of groups in the regular expression *) 
}

(* ?? *)
type category = int

(*****************************************************************************)
(* Category *)
(*****************************************************************************)
let cat_inexistant = 1
let cat_letter = 2
let cat_not_letter = 4
let cat_newline = 8
let cat_lastnewline = 16
let cat_search_boundary = 32

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)
let mk_re init cols col_repr ncol lnl group_count =
  { initial = init;
    initial_states = [];
    cols = cols;
    col_repr = col_repr;
    ncol = ncol;
    lnl = lnl;
    tbl = Automata.create_working_area ();
    states = Automata.States.create 97;
    group_count = group_count }

(*****************************************************************************)
(* Compilation helpers *)
(*****************************************************************************)

let rec cset_hash_rec l =
  match l with
    []        -> 0
  | (i, j)::r -> i + 13 * j + 257 * cset_hash_rec r
(* ?? let cset_hash l = (cset_hash_rec l) land 0x3FFFFFFF *)

(*
module CSetMap =
  Map.Make
  (struct
    type t = int * (int * int) list
    let compare (i, u) (j, v) =
      let c = compare i j in if c <> 0 then c else compare u v
   end)
*)
module CSetMap = struct
  let find a b = Map_.find a b
  let add a b c = Map_.add a b c

  type t_key = int * (int * int) list
  type 'a _tTODO = (t_key, 'a) Map_.t

  let empty = Map_.empty
  (* TODO? the tests seems to pass with general Pervasives.compare
     so we can just use Map_ for now
   *)
(*
    let compare (i, u) (j, v) =
      let c = compare i j in if c <> 0 then c else compare u v
*)
end

(* copy paste of Regexp.ml helpers *)
let csingle c = Cset.single (Char.code c)
let cseq c c' = Cset.seq (Char.code c) (Char.code c')

let trans_set cache (cm : bytes) s =
  match s with
    [i, j] when i = j ->
      csingle (Bytes.get cm i)
  | _ ->
      let v = (cset_hash_rec s, s) in
      try
        CSetMap.find v !cache
      with Not_found ->
        let l =
          List.fold_right
            (fun (i, j) l -> Cset.union (cseq (Bytes.get cm i) (Bytes.get cm j)) l)
            s Cset.empty
        in
        cache := CSetMap.add v l !cache;
        l

(*****************************************************************************)
(* Colormap *)
(*****************************************************************************)

(*XXX Use a better algorithm allowing non-contiguous regions? *)
let rec split s (cm : bytes) =
  match s with
    []    -> ()
  | (i, j)::r -> Bytes.set cm i '\001'; Bytes.set cm (j + 1) '\001'; split r cm

let cadd c s = Cset.add (Char.code c) s

let calpha = cadd '\170' (cadd '\186' (Cset.union clower cupper))
let cdigit = cseq '0' '9'
let calnum = Cset.union calpha cdigit

let cword = cadd '_' calnum

let colorize (c : bytes) (regexp : Regexp.t) =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
      Set s                     -> split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> split (csingle '\n') c
    | Beg_of_word | End_of_word
    | Not_bound                 -> split cword c
    | Beg_of_str | End_of_str
    | Start | Stop              -> ()
    | Last_end_of_line          -> lnl := true
    (* ocaml-light: was factorized before *)
    | Sem (_, r)                   -> colorize r
    | Sem_greedy (_, r)                   -> colorize r
    | Group r                    -> colorize r
    | No_group r                   -> colorize r
    | Nest r                    -> colorize r
    | Case _ | No_case _
    | Intersection _
    | Complement _
    | Difference _              -> assert false
  in
  colorize regexp;
  !lnl

let make_cmap () = Bytes.make 257 '\000'

let flatten_cmap (cm : bytes) =
  let c = Bytes.create 256 in
  let col_repr = Bytes.create 256 in
  let v = ref 0 in
  Bytes.set c 0 '\000';
  Bytes.set col_repr 0 '\000';
  for i = 1 to 255 do
    if Bytes.get cm i <> '\000' then incr v;
    Bytes.set c i (Char.chr !v);
    Bytes.set col_repr !v (Char.chr i)
  done;
  (c, Bytes.sub_string col_repr 0 (!v + 1), !v + 1)

(*****************************************************************************)
(* Compilation *)
(*****************************************************************************)

let sequence l =
  match l with
    [x] -> x
  | l   -> Sequence l

let rec merge_sequences l =
  match l with
    [] ->
      l
  | Alternative l' :: r ->
      merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
      begin match merge_sequences r with
        Sequence (x' :: y') :: r' when x = x' ->
          Sequence [x; Alternative [sequence y; sequence y']] :: r'
      | r' ->
          Sequence (x :: y) :: r'
      end
  | x :: r ->
      x :: merge_sequences r

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
    A.First, A.First -> cr
  | A.First, k       -> A.seq ids k cr (A.eps ids)
  |  _               -> cr

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(* XXX should probably compute a category mask *)
let rec translate ids kind ign_group ign_case greedy pos cache (c : bytes) r =
  match r with
    Set s ->
      (A.cst ids (trans_set cache c s), kind)
  | Sequence l ->
      (trans_seq ids kind ign_group ign_case greedy pos cache c l, kind)
  | Alternative l ->
      begin match merge_sequences l with
        [r'] ->
          let (cr, kind') =
            translate ids kind ign_group ign_case greedy pos cache c r' in
          (enforce_kind ids kind kind' cr, kind)
      | _l' ->
          (A.alt ids
             (List.map
                (fun r' ->
                   let (cr, kind') =
                     translate ids kind ign_group ign_case greedy
                       pos cache c r' in
                   enforce_kind ids kind kind' cr)
                (merge_sequences l)),
           kind)
      end
  | Repeat (r', i, j) ->
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r' in
      let rem =
        match j with
          None ->
            A.rep ids greedy kind' cr
        | Some j ->
            let f =
              match greedy with
                A.Greedy ->
                  fun rem ->
                    A.alt ids
                      [A.seq ids kind' (A.rename ids cr) rem; A.eps ids]
              | A.Non_greedy ->
                  fun rem ->
                    A.alt ids
                      [A.eps ids; A.seq ids kind' (A.rename ids cr) rem]
            in
            iter (j - i) f (A.eps ids)
      in
      (iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind)
  | Beg_of_line ->
      (A.after ids (cat_inexistant lor cat_newline), kind)
  | End_of_line ->
      (A.before ids (cat_inexistant lor cat_newline), kind)
  | Beg_of_word ->
      (A.seq ids A.First
           (A.after ids (cat_inexistant lor cat_not_letter))
           (A.before ids (cat_inexistant lor cat_letter)),
       kind)
  | End_of_word ->
      (A.seq ids A.First
           (A.after ids (cat_inexistant lor cat_letter))
           (A.before ids (cat_inexistant lor cat_not_letter)),
       kind)
  | Not_bound ->
      (A.alt ids [A.seq ids A.First
                    (A.after ids cat_letter)
                    (A.before ids cat_letter);
                  A.seq ids A.First
                    (A.after ids cat_letter)
                    (A.before ids cat_letter)],
       kind)
  | Beg_of_str ->
      (A.after ids cat_inexistant, kind)
  | End_of_str ->
      (A.before ids cat_inexistant, kind)
  | Last_end_of_line ->
      (A.before ids (cat_inexistant lor cat_lastnewline), kind)
  | Start ->
      (A.after ids cat_search_boundary, kind)
  | Stop ->
      (A.before ids cat_search_boundary, kind)
  | Sem (kind', r') ->
      let (cr, kind'') =
        translate ids kind' ign_group ign_case greedy pos cache c r' in
      (enforce_kind ids kind' kind'' cr,
       kind')
  | Sem_greedy (greedy', r') ->
      translate ids kind ign_group ign_case greedy' pos cache c r'
  | Group r' ->
      if ign_group then
        translate ids kind ign_group ign_case greedy pos cache c r'
      else
        let p = !pos in
        pos := !pos + 2;
        let (cr, kind') =
          translate ids kind ign_group ign_case greedy pos cache c r' in
        (A.seq ids A.First (A.mark ids p) (
         A.seq ids A.First cr (A.mark ids (p + 1))),
         kind')
  | No_group r' ->
      translate ids kind true ign_case greedy pos cache c r'
  | Nest r' ->
      let b = !pos in
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r'
      in
      let e = !pos - 1 in
      if e < b then
        (cr, kind')
      else
        (A.seq ids A.First (A.erase ids b e) cr, kind')
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ ->
      assert false

and trans_seq ids kind ign_group ign_case greedy pos cache c l =
  match l with
    [] ->
      A.eps ids
  | [r] ->
      let (cr', kind') =
        translate ids kind ign_group ign_case greedy pos cache c r in
      enforce_kind ids kind kind' cr'
  | r :: rem ->
      let (cr', kind') =
        translate ids kind ign_group ign_case greedy pos cache c r in
      let cr'' =
        trans_seq ids kind ign_group ign_case greedy pos cache c rem in
      if A.def cr'' = A.Eps then
        cr'
      else if A.def cr' = A.Eps then
        cr''
      else
        A.seq ids kind' cr' cr''

(*****************************************************************************)
(* Case *)
(*****************************************************************************)

let case_insens s =
  Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                   (Cset.offset (-32) (Cset.inter s clower)))

let as_set r =
  match r with
    Set s -> s
  | _     -> assert false

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case r =
  match r with
    Set s ->
      Set (if ign_case then case_insens s else s)
  | Sequence l ->
      Sequence (List.map (handle_case ign_case) l)
  | Alternative l ->
      let l' = List.map (handle_case ign_case) l in
      if is_charset (Alternative l') then
        Set (List.fold_left (fun s r -> Cset.union s (as_set r)) Cset.empty l')
      else
        Alternative l'
  | Repeat (r, i, j) ->
      Repeat (handle_case ign_case r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop ->
      r
  | Sem (k, r) ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Sem (k, r')
  | Sem_greedy (k, r) ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Sem_greedy (k, r')
  | Group r ->
      Group (handle_case ign_case r)
  | No_group r ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      No_group r'
  | Nest r ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Nest r'
  | Case r ->
      handle_case false r
  | No_case r ->
      handle_case true r
  | Intersection l ->
      let l' = List.map (fun r -> handle_case ign_case r) l in
      Set (List.fold_left (fun s r -> Cset.inter s (as_set r)) cany l')
  | Complement l ->
      let l' = List.map (fun r -> handle_case ign_case r) l in
      Set (Cset.diff cany
             (List.fold_left (fun s r -> Cset.union s (as_set r))
                Cset.empty l'))
  | Difference (r, r') ->
      Set (Cset.inter (as_set (handle_case ign_case r))
             (Cset.diff cany (as_set (handle_case ign_case r'))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let compile_1 (regexp : t) : re =
  let regexp = handle_case false regexp in
  let c = make_cmap () in
  let need_lnl = colorize c regexp in
  let (col, col_repr, ncol) = flatten_cmap c in
  let lnl = if need_lnl then ncol else -1 in
  let ncol = if need_lnl then ncol + 1 else ncol in
  let ids = A.create_ids () in
  let pos = ref 0 in
  let (r, kind) =
    translate ids
      A.First false false A.Greedy pos (ref CSetMap.empty) col regexp in
  let r = enforce_kind ids A.First kind r in
(*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re r (Bytes.to_string col) col_repr ncol lnl (!pos / 2)

let compile (r : t) : re = compile_1 (seq [shortest (rep any); group r])
