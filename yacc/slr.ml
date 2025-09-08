(*s: yacc/slr.ml *)
(*s: copyright ocamlyacc *)
(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: copyright ocamlyacc *)
open Ast
open Lr0
open Lrtables

module Set = Set_
module Map = Map_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Computing the SLR(1) tables for a context free grammar using
 * the algorithm described in the dragon book in chapter 4.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Slr.filter_some]](yacc) *)
(* from my common.ml *)
let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l
(*e: function [[Slr.filter_some]](yacc) *)

(*s: function [[Slr.map_filter]](yacc) *)
let map_filter f xs = xs |> List.map f |> filter_some
(*e: function [[Slr.map_filter]](yacc) *)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Slr.lr_tables]](yacc) *)
let lr_tables env auto follow =
  let trans = auto.trans |> Map.to_list in

  let action_tables =
    Map.fold (fun items stateid acc ->
      Set.fold (fun item acc ->
        let (R ridx, didx) = item in
        let r = env.g.(ridx) in
        match Lr0.after_dot r didx with
        (* a shift *)
        | Some (Term t) -> 
            let items2 = Map.find (items, (Term t)) auto.trans in
            let dst = Map.find items2 auto.state_to_int in
            ((stateid, t), Shift dst)::acc
        | Some (Nonterm _) -> acc
        (* a reduction *)
        | None -> 
            if r.lhs = Ast.start_nonterminal
            then ((stateid, Ast.dollar_terminal), Accept)::acc
            else
              let terms = Map.find r.lhs follow in
              let xs = Set.elements terms in
              (xs |> List.map (fun t -> (stateid, t), Reduce (R ridx))) @ acc
      ) items acc
    ) auto.state_to_int []
  in


  let goto_tables =
    trans |> map_filter (fun ((items1, symb), items2) ->
      match symb with
      | Nonterm nt ->
        let src = Map.find items1 auto.state_to_int in
        let dst = Map.find items2 auto.state_to_int in
        Some ((src, nt), dst)
      | _ -> None
    )
  in

  action_tables, goto_tables
(*e: function [[Slr.lr_tables]](yacc) *)
(*e: yacc/slr.ml *)
