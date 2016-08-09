(*s: yacc/output.ml *)
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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: constant Output.copy_buffer (yacc) *)
let copy_buffer = String.create 1024
(*e: constant Output.copy_buffer (yacc) *)

(*s: function Output.get_chunk (yacc) *)
let get_chunk ic (Location(start,stop)) =
  seek_in ic start;
  let buf = Buffer.create 1024 in

  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    Buffer.add_string buf (String.sub copy_buffer 0 m);
    n := !n - m
  done;
  Buffer.contents buf
(*e: function Output.get_chunk (yacc) *)

(*s: function Output.copy_chunk (yacc) *)
let copy_chunk ic oc (Location(start,stop)) =
  seek_in ic start;
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done
(*e: function Output.copy_chunk (yacc) *)

(*s: function Output.replace_dollar_underscore (yacc) *)
(* actually does the replacment in place 
 * less: we could use Str instead, but this adds a dependency.
 *  Str.global_replace (Str.regexp_string "$") "_" s
 *)
let replace_dollar_underscore s =
  let rec aux startpos =
    try
      let idx = String.index_from s startpos '$' in
      String.set s idx '_';
      aux (idx+1)
    with Not_found -> ()
  in
  aux 0;
  s
(*e: function Output.replace_dollar_underscore (yacc) *)

(*s: constant Output.spf (yacc) *)
let spf = Printf.sprintf
(*e: constant Output.spf (yacc) *)

(*s: function Output.int_of_char (yacc) *)
(* todo: what about $22? what error message give if type and $x ? *)
let int_of_char c =
  let i = Char.code c in
  if i <= Char.code '9' && i >= Char.code '1'
  then i - Char.code '0'
  else failwith (spf "the characted %c is not a char" c)
(*e: function Output.int_of_char (yacc) *)

(*s: function Output.extract_dollars_set (yacc) *)
let extract_dollars_set s =
  let set = ref Set.empty in
  let rec aux startpos =
    try
      let idx = String.index_from s startpos '$' in
      let c = String.get s (idx + 1) in
      set := Set.add (int_of_char c) !set;
      aux (idx + 2)
    with Not_found | Invalid_argument _ | Failure _ -> ()
  in
  aux 0;
  !set
(*e: function Output.extract_dollars_set (yacc) *)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function Output.output_parser (yacc) *)
let output_parser def env lrtables ic oc =
  let pf x = Printf.fprintf oc x in
  let (action_table, goto_table) = lrtables in

  let htype = Hashtbl.create 13 in
  def.directives |> List.iter (function
    | Token (Some s, t) -> Hashtbl.add htype (Term t) s
    | Type (s, nt) -> Hashtbl.add htype (Nonterm nt) s
    | _ -> ()
  );

  pf "type token =\n";
  def.directives |> List.iter (function
    | Token (sopt, (T s)) ->
      pf " | %s%s\n" s
        (match sopt with
        | None -> ""
        | Some s -> spf " of %s" s
        )
    | _ -> ()
  );

  copy_chunk ic oc def.header;
  pf "\n";
  pf "let user_actions = [|\n";
  env.g |> Array.iteri (fun i r ->
    if i = 0
    then pf "  (fun __parser_env -> failwith \"parser\");\n"
    else begin
      let s = get_chunk ic r.act in
      (* ugly: right now have to be before replace_dollar_underscore
       * because replace_dollar_underscore does side effect on s
       *)
      let dollars = extract_dollars_set s in
      let s' = replace_dollar_underscore s in
      let symbs = Array.of_list (Nonterm (NT "_fake_"):: r.rhs) in
      pf "  (fun __parser_env -> \n";
      dollars |> Set.iter (fun i ->
        pf "     let _%d = (Parsing.peek_val_simple __parser_env %d : %s) in\n"
          i i
          (* type info on terminal or on non terminal *)
          (let symb = symbs.(i) in
           if not (Hashtbl.mem htype symb)
           then
             (match symb with
             | Nonterm (NT s) -> "'" ^ s
             | Term _ -> failwith "you try to access a token with no value"
             )
           else Hashtbl.find htype symb
          )
      );

      pf "    Obj.repr((\n";
      pf "      "; 
      pf "%s" s';
      pf "   )";
      if Hashtbl.mem htype (Nonterm r.lhs)
      then pf ": %s" (Hashtbl.find htype (Nonterm r.lhs))
      else ();
      pf ")\n";
      pf "   );\n";
    end
  );
  pf "|]\n";
  copy_chunk ic oc def.trailer;


  pf "\n";
  pf "open Parsing\n";

  (* for debugging support *)
  pf "let string_of_token = function\n";
  def.directives |> List.iter (function
    | Token (sopt, (T s)) ->
      pf " | %s%s -> \"%s\"\n" s
        (match sopt with
        | None -> ""
        | Some s -> " _"
        )
        s
    | _ -> ()
  );
  pf "\n";

  (* the main tables *)
  pf "let lrtables = {\n";

  (* the action table *)
  pf "  action = (function\n";
  action_table |> List.iter (fun ((S id, T t), action) ->
    (* if reached a state where there is dollar involved, means
     * we're ok!
     *)
    if t = "$"
    then begin 
      (* in practice the '_' will be the TEOF token returned another time
       * by the lexer.
       *)
      pf "   | S %d, _ -> " id;
      (match action with
      | Reduce (R ridx) ->
          let r = env.g.(ridx) in
          let n = List.length r.rhs in
          let (NT l) = r.lhs in
          pf "Reduce (NT \"%s\", %d, RA %d)" l n ridx
      | Accept -> pf "Accept"
      | _ -> failwith "impossible to have a non reduce or accept action on $"
      );
      pf "\n";
    end
    else begin
      pf "   | S %d, %s%s -> " id t
        (if Hashtbl.mem htype (Term (T t)) 
         then " _"
         else ""
        );
      (match action with
      | Shift (S id) -> pf "Shift (S %d)" id
      | Accept -> pf "Accept"
      | Reduce (R ridx) ->
          let r = env.g.(ridx) in
          let n = List.length r.rhs in
          let (NT l) = r.lhs in
          pf "Reduce (NT \"%s\", %d, RA %d)" l n ridx
      | Error -> failwith "Error should not be in action tables"
      );
      pf "\n";
    end
  );

  pf "    | _ -> raise Parse_error\n";
  pf "  );\n";


  (* the goto table *)
  pf "  goto = (function\n";
  goto_table |> List.iter (fun ((S id1, NT nt), S id2) ->
    pf "  | S %d, NT \"%s\" -> S %d\n" id1 nt id2
  );
  pf "    | _ -> raise Parse_error\n";
  pf "  );\n";
  pf "}\n";


  (* the main entry point *)
  let nt = Ast.start_symbol def in
  let (NT start) = nt in

  pf "let %s lexfun lexbuf =\n" start;
  pf "  Parsing.yyparse_simple lrtables user_actions lexfun string_of_token lexbuf\n";
  ()
(*e: function Output.output_parser (yacc) *)

(*e: yacc/output.ml *)
