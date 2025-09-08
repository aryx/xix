(*s: yacc/first_follow.ml *)
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
open Lr0 (* for the augmented grammar *)

module Set = Set_
module Map = Map_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Computing the first and follow set for a context free grammar, using
 * the algorithm described in the dragon book in chapter 4.
 * 
 * The only difference with the dragon book is that I've split their
 * FIRST into a first map and an epsilon set, so epsilon is never
 * in first.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[First_follow.first]](yacc) *)
type first = (Ast.symbol, Ast.term Set_.t) Map_.t
(*e: type [[First_follow.first]](yacc) *)

(*s: type [[First_follow.epsilon]](yacc) *)
type epsilon = Ast.nonterm Set_.t
(*e: type [[First_follow.epsilon]](yacc) *)

(*s: type [[First_follow.follow]](yacc) *)
type follow = (Ast.nonterm, Ast.term Set_.t) Map_.t
(*e: type [[First_follow.follow]](yacc) *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[First_follow.first_of_sequence]](yacc) *)
let rec first_of_sequence (first, epsilon) xs =
  match xs with
  | [] -> Set.empty
  | x::xs ->
      let set = Map.find x first in
      (match x with
      | Term _ -> set
      | Nonterm nt ->
          if Set.mem nt epsilon
          then Set.union set (first_of_sequence (first, epsilon) xs)
          else set
      )
(*e: function [[First_follow.first_of_sequence]](yacc) *)

(*s: function [[First_follow.epsilon_of_sequence]](yacc) *)
let rec epsilon_of_sequence epsilon xs =
  match xs with
  | [] -> true
  | x::xs ->
      (match x with
      | Term _ -> false
      | Nonterm nt ->
          Set.mem nt epsilon && epsilon_of_sequence epsilon xs
      )
(*e: function [[First_follow.epsilon_of_sequence]](yacc) *)

(*****************************************************************************)
(* Algorithms *)
(*****************************************************************************)

(*s: function [[First_follow.compute_first]](yacc) *)
let compute_first grm =
  (* faster to use an hashtbl? could use Hashtbl.replace which would
   * be faster?
   *)
  let first = ref Map.empty in
  let epsilon = ref Set.empty in

  (* initialize first *)
  grm |> List.iter (fun r ->
    if not (Map.mem (Nonterm r.lhs) !first)
    then first := !first |> Map.add (Nonterm r.lhs) Set.empty ;
    r.rhs |> List.iter (function
      | Term t ->
        if not (Map.mem (Term t) !first)
        then first := !first |> Map.add (Term t) (Set.singleton t);
      | Nonterm _  -> ()
    )
  );

  (* fixpoint *)
  let added = ref true in
  while !added do
    added := false;

    grm |> List.iter (fun r ->
      let lhs = Nonterm r.lhs in
      if r.rhs = [] && not (Set.mem r.lhs !epsilon)
      then begin 
        added := true;
        epsilon := Set.add r.lhs !epsilon;
      end;

      let rec aux all_before_are_nullable xs =
        match xs with
        | [] -> 
            if all_before_are_nullable && not (Set.mem r.lhs !epsilon)
            then begin 
              epsilon := Set.add r.lhs !epsilon;
              added := true;
            end
        | x::xs when all_before_are_nullable ->
            (* less: if check.ml use/def check has been done correctly,
             * we should never get some Not_found here
             *)
             let first_x = try Map.find x !first with Not_found -> Set.empty in
             let old = try Map.find lhs !first with Not_found -> Set.empty in
             if not (Set.is_empty first_x) && 
                not (Set.subset first_x old)
             then begin 
               first := !first |> Map.add(Nonterm r.lhs)(Set.union old first_x);
               added := true
             end;
             (match x with
             | Term _ -> (* we can stop here *) ()
             | Nonterm nt ->
                if Set.mem nt !epsilon
                then aux true xs
             )
        | _ -> ()
      in
      aux true r.rhs
    )
  done;
  !first, !epsilon
(*e: function [[First_follow.compute_first]](yacc) *)

(*s: function [[First_follow.compute_follow]](yacc) *)
(* assumes augmented grammar *)
let compute_follow env (first, epsilon) =
  let follow = ref Map.empty in

  (* initialize first *)
  env.g |> Array.iter (fun r ->
    if not (Map.mem r.lhs !follow)
    then follow := !follow |> Map.add r.lhs Set.empty ;
  );
  (* follow($S) = { $ } *)
  follow := !follow |> Map.add Ast.start_nonterminal 
      (Set.singleton Ast.dollar_terminal);

  (* fixpoint *)
  let added = ref true in
  while !added do
    added := false;

    env.g |> Array.iter (fun r ->
      let rec aux xs =
        match xs with
        | [] -> ()
        | x::beta ->
          (match x with
          (* A -> alpha B beta *)
          | Nonterm b ->
              let set = first_of_sequence (first, epsilon) beta in
              let oldb = Map.find b !follow in
              if not (Set.is_empty set) &&
                 not (Set.subset set oldb) then begin
                  follow := !follow |> Map.add b (Set.union oldb set);
                  added := true
                end;
              if epsilon_of_sequence epsilon beta
              then begin
                let oldb = Map.find b !follow in
                let follow_a = Map.find r.lhs !follow in
                if not (Set.is_empty follow_a) &&
                   not (Set.subset follow_a oldb) then begin
                     follow := !follow |> Map.add b (Set.union oldb follow_a);
                     added := true
                   end;
              end;
              aux beta
          | Term _ -> aux beta
          )
      in
      aux r.rhs
    )
  done;
  !follow
(*e: function [[First_follow.compute_follow]](yacc) *)
(*e: yacc/first_follow.ml *)
