(*s: yacc/dump.ml *)
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
open Stdcompat (* for |> *)
open Format

open Ast
open Lr0

module Set = Set_
module Map = Map_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: does not indent things correctly, even after an
 * open_box 2; I don't understand
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* common.ml *)
(*s: type [[Dump.either]](yacc) *)
type ('a,'b) either = Left of 'a | Right of 'b
(*e: type [[Dump.either]](yacc) *)

(*s: function [[Dump.partition_either]](yacc) *)
let partition_either f l =
  let rec part_either left right = function
  | [] -> (List.rev left, List.rev right)
  | x :: l ->
      (match f x with
      | Left  e -> part_either (e :: left) right l
      | Right e -> part_either left (e :: right) l) in
  part_either [] [] l
(*e: function [[Dump.partition_either]](yacc) *)

(*s: function [[Dump.hash_of_list]](yacc) *)
let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h
(*e: function [[Dump.hash_of_list]](yacc) *)



(*s: function [[Dump.string_of_symbol]](yacc) *)
let string_of_symbol s =
  match s with
  (* in ocamlyacc terminals are constructors and so are uppercase and
   * so non terminals are lowercase, but it's the opposite convention
   * used in the dragon book, so here we dump in the dragon book
   * way so it's easier to compare what we generate with what
   * the dragon book says we should generate
   *)
  | Nonterm (NT s) -> String.uppercase_ascii s
  | Term (T s) ->
    (match s with
    (* special cases for arith.mly and tests.ml grammar toy examples *)
    | "PLUS" -> "+"
    | "MULT" -> "*"
    | "TOPAR" -> "("
    | "TCPAR" -> ")"

    | _ -> String.lowercase_ascii s
    )
(*e: function [[Dump.string_of_symbol]](yacc) *)

(*s: constant [[Dump.pf]](yacc) *)
let pf = Printf.printf
(*e: constant [[Dump.pf]](yacc) *)
(*s: constant [[Dump.spf]](yacc) *)
let spf = Printf.sprintf
(*e: constant [[Dump.spf]](yacc) *)

(*s: function [[Dump.string_of_action]](yacc) *)
let string_of_action x =
  match x with
  | Lrtables.Shift (S d) -> spf "s%d" d
  | Lrtables.Reduce (R d) -> spf "r%d" d
  | Lrtables.Accept -> spf "acc"
  | Lrtables.Error -> ""
(*e: function [[Dump.string_of_action]](yacc) *)


(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(*s: function [[Dump.dump_symbol]](yacc) *)
let dump_symbol s =
  print_string (string_of_symbol s)
(*e: function [[Dump.dump_symbol]](yacc) *)

(*s: function [[Dump.dump_item]](yacc) *)
let dump_item env item =
  let (R idx, D didx) = item in
  let r = env.g.(idx) in

  dump_symbol (Nonterm r.lhs);
  print_space ();
  print_string "->";
  open_box 0;
  print_space ();
  r.rhs |> Array.of_list |> Array.iteri (fun i s ->
    if i = didx
    then begin 
      print_string ".";
      print_space ();
    end;
    dump_symbol s;
    print_space ();
  );
  if didx = List.length r.rhs then begin
      print_string ".";
  end;

  close_box ();
(*  print_space (); print_string "(R"; print_int idx; print_string ")" *)
  ()
(*e: function [[Dump.dump_item]](yacc) *)

(*s: function [[Dump.dump_items]](yacc) *)
let dump_items env items =
  items 
  |> Set.elements |> List.sort (fun (R a, _) (R b, _) -> a - b)
  |> List.iter (fun item -> 
    open_box 0;
    dump_item env item;
    close_box ();
    print_newline ();
  )
(*e: function [[Dump.dump_items]](yacc) *)

(*s: function [[Dump.dump_lr0_automaton]](yacc) *)
let dump_lr0_automaton env auto =

  open_box 0;

  (* the states *)
  auto.int_to_state |> Array.iteri (fun i items ->
    print_string "I"; print_int i; print_newline ();
    open_box 2;
    dump_items env items;
    close_box ();
    print_newline ();
  );

  (* the transitions *)
  auto.trans |> Map.iter (fun (items1, symb) items2 ->
    let (S src) = Map.find items1 auto.state_to_int in
    let (S dst) = Map.find items2 auto.state_to_int in
    print_string "I"; print_int src;
    print_string " --"; dump_symbol symb; print_string "-->";
    print_string " I"; print_int dst;
    print_newline ()
  );

  close_box ()
(*e: function [[Dump.dump_lr0_automaton]](yacc) *)


(*s: function [[Dump.dump_lrtables]](yacc) *)
let dump_lrtables env lrtables =
  let symbols = Lr0.all_symbols env in
  let (action_table, goto_table) = lrtables in
  let haction = hash_of_list action_table in
  let hgoto = hash_of_list goto_table in

  let (terms, nonterms) = 
    symbols |> Set.elements |> partition_either (function
      | Term t -> Left t
      | Nonterm nt -> Right nt
    )
  in
  let terms = terms @ [Ast.dollar_terminal] in
  let max_state = 
    action_table |> List.fold_left (fun acc (((S stateid), _), _) ->
      max acc stateid
    ) 0
  in
  
  (* print headers *)
  pf "   ";
  terms |> List.iter (fun t ->
    let s = string_of_symbol (Term t) in
    pf "%3s " s;
  );
  pf "  ";
  nonterms |> List.iter (fun nt ->
    let s = string_of_symbol (Nonterm nt) in
    pf "%3s " s;
  );
  pf "\n";

  let conflicts = ref [] in
  
  for i = 0 to max_state do
    pf "%2d " i;
    terms |> List.iter (fun t ->
      let xs = Hashtbl.find_all haction (S i, t) in
      (match xs with
      | [] -> pf "%3s " " "
      | [x] -> pf "%3s " (string_of_action x)
      | x::xs -> 
        pf "%3s " "!?!";
        conflicts := (S i, t, x::xs)::!conflicts
      );
    );
    pf "  ";
    nonterms |> List.iter (fun nt ->
      let xs = Hashtbl.find_all hgoto (S i, nt) in
      (match xs with
      | [] -> pf "%3s " " "
      | [S d] -> pf "%3d " d
      | x::xs -> pf "%3s " "!?!"
      );
    );

    pf "\n";
  done;
  pf "\n";
  pf "%d conflicts\n" (List.length !conflicts);
  pf "\n";
  ()
(*e: function [[Dump.dump_lrtables]](yacc) *)

(*e: yacc/dump.ml *)
