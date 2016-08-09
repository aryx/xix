(*s: lex/lexgen.ml *)
(*s: copyright ocamllex *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright ocamllex *)
(* Compiling a lexer definition *)

open Ast
module Set = Setx
module Map = Mapx

(*s: type Lexgen.action_id *)
type action_id = int
(*e: type Lexgen.action_id *)

(*s: type Lexgen.charset_id *)
type charset_id = int
(*e: type Lexgen.charset_id *)

(*s: type Lexgen.regexp *)
(* Deep abstract syntax for regular expressions *)

type regexp =
    Empty
  | Chars of charset_id
  | Action of action_id
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
(*e: type Lexgen.regexp *)

(*s: type Lexgen.lexer_entry *)
type lexer_entry =
  { lex_name: string;
    lex_regexp: regexp;
    lex_actions: (action_id * Ast.action) list;
  }
(*e: type Lexgen.lexer_entry *)
    
(* Representation of automata *)

(*s: type Lexgen.automata *)
type automata_row =
    Perform of action_id
  (* indexed by an integer between 0 and 256(eof), that is a char_ *)
  | Shift of automata_trans * automata_move array 
(*e: type Lexgen.automata *)
(*s: type Lexgen.automata_trans *)
and automata_trans =
    No_remember
  | Remember of action_id
(*e: type Lexgen.automata_trans *)
(*s: type Lexgen.automata_move *)
and automata_move =
    Backtrack
  | Goto of int
(*e: type Lexgen.automata_move *)

(*s: type Lexgen.automata_entry *)
(* Representation of entry points *)

type automata_entry =
  { auto_name: string;
    auto_initial_state: int;
    auto_actions: (action_id * Ast.action) list;
  }
(*e: type Lexgen.automata_entry *)

(*s: type Lexgen.automata_matrix *)
(* indexed by state number *)
type automata_matrix = automata_row array
(*e: type Lexgen.automata_matrix *)
    
(* From shallow to deep syntax *)

(*s: constant Lexgen.chars *)
let chars = ref ([] : char_ list list)
(*e: constant Lexgen.chars *)
(*s: constant Lexgen.chars_count *)
let chars_count = ref (0: charset_id)
(*e: constant Lexgen.chars_count *)
(*s: constant Lexgen.actions *)
let actions = ref ([] : (action_id * Ast.location) list)
(*e: constant Lexgen.actions *)
(*s: constant Lexgen.actions_count *)
let actions_count = ref (0: action_id)
(*e: constant Lexgen.actions_count *)

(*s: function Lexgen.encode_regexp *)
let rec encode_regexp = function
    Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in
      incr chars_count;
      chars := cl :: !chars;
      Chars(n)
  | Sequence(r1,r2) ->
      Seq(encode_regexp r1, encode_regexp r2)
  | Alternative(r1,r2) ->
      Alt(encode_regexp r1, encode_regexp r2)
  | Repetition r ->
      Star (encode_regexp r)
(*e: function Lexgen.encode_regexp *)

(*s: function Lexgen.encode_casedef *)
let encode_casedef casedef =
  casedef |> List.fold_left (fun reg (re, action) ->
     let act_num = !actions_count in
     incr actions_count;
     actions := (act_num, action) :: !actions;
     Alt(reg, Seq(encode_regexp re, Action act_num))
  ) Empty
(*e: function Lexgen.encode_casedef *)

(*s: function Lexgen.encode_lexdef *)
let encode_lexdef def =
  chars := [];
  chars_count := 0;
(* CONFIG   actions_count := 0; *)
  let entries =
    def.entrypoints |> List.map (fun (entry_name, casedef) ->
        actions := [];
        (* CONFIG !! for simpler output can't do that *)
        actions_count := 0;
        let re = encode_casedef casedef in
        { lex_name = entry_name;
          lex_regexp = re;
          lex_actions = List.rev !actions }
     )
  in
  (* map a charset_id to a charset *)
  let charsets = Array.of_list (List.rev !chars) in
  (charsets, entries)
(*e: function Lexgen.encode_lexdef *)


(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3 *)

(*s: type Lexgen.transition *)
type transition =
    OnChars of charset_id
  | ToAction of action_id
(*e: type Lexgen.transition *)

(*s: type Lexgen.state *)
type state = transition Set.t
(*e: type Lexgen.state *)

(*s: function Lexgen.nullable *)
let rec nullable = function
    Empty      -> true
  | Chars _    -> false
  | Action _   -> false
  | Seq(r1,r2) -> nullable r1 && nullable r2
  | Alt(r1,r2) -> nullable r1 || nullable r2
  | Star r     -> true
(*e: function Lexgen.nullable *)

(*s: function Lexgen.firstpos *)
let rec firstpos = function
    Empty      -> Set.empty
  | Chars pos  -> Set.add (OnChars pos) Set.empty
  | Action act -> Set.add (ToAction act) Set.empty
  | Seq(r1,r2) -> if nullable r1
                  then Set.union (firstpos r1) (firstpos r2)
                  else firstpos r1
  | Alt(r1,r2) -> Set.union (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r
(*e: function Lexgen.firstpos *)

(*s: function Lexgen.lastpos *)
let rec lastpos = function
    Empty      -> Set.empty
  | Chars pos  -> Set.add (OnChars pos) Set.empty
  | Action act -> Set.add (ToAction act) Set.empty
  | Seq(r1,r2) -> if nullable r2
                  then Set.union (lastpos r1) (lastpos r2)
                  else lastpos r2
  | Alt(r1,r2) -> Set.union (lastpos r1) (lastpos r2)
  | Star r     -> lastpos r
(*e: function Lexgen.lastpos *)

(*s: function Lexgen.followpos *)
let followpos size_charsets entries =
  let v = Array.create size_charsets Set.empty in
  let fill_pos first = function
      OnChars pos -> v.(pos) <- Set.union first v.(pos)
    | ToAction _  -> () 
  in
  let rec fill = function
      Seq(r1,r2) ->
        fill r1; fill r2;
        Set.iter (fill_pos (firstpos r2)) (lastpos r1)
    | Alt(r1,r2) ->
        fill r1; fill r2
    | Star r ->
        fill r;
        Set.iter (fill_pos (firstpos r)) (lastpos r)
    | _ -> () in
  entries |> List.iter (fun entry -> fill entry.lex_regexp);
  v
(*e: function Lexgen.followpos *)

(*s: constant Lexgen.no_action *)
let no_action = (max_int: action_id)
(*e: constant Lexgen.no_action *)

(*s: function Lexgen.split_trans_set *)
let split_trans_set trans_set =
  Set.fold (fun trans (act, pos_set as act_pos_set) ->
      match trans with
        OnChars pos -> (act, pos :: pos_set)
      | ToAction act1 -> if act1 < act then (act1, pos_set) else act_pos_set
   ) trans_set (no_action, [])
(*e: function Lexgen.split_trans_set *)

(*s: constant Lexgen.state_map *)
let state_map = ref (Map.empty: (state, int) Map.t)
(*e: constant Lexgen.state_map *)
(*s: constant Lexgen.todo *)
let todo = (Stack.create() : (state * int) Stack.t)
(*e: constant Lexgen.todo *)
(*s: constant Lexgen.next_state_num *)
let next_state_num = ref 0
(*e: constant Lexgen.next_state_num *)

(*s: function Lexgen.reset_state_mem *)
let reset_state_mem () =
  state_map := Map.empty;
  Stack.clear todo;
  next_state_num := 0
(*e: function Lexgen.reset_state_mem *)

(*s: function Lexgen.get_state *)
let get_state st = 
  try
    Map.find st !state_map
  with Not_found ->
    let num = !next_state_num in
    incr next_state_num;
    state_map := Map.add st num !state_map;
    Stack.push (st, num) todo;
    num
(*e: function Lexgen.get_state *)

(*s: function Lexgen.map_on_all_states *)
let map_on_all_states f =
  let res = ref [] in
  begin try
    while true do
      let (st, i) = Stack.pop todo in
      let r = f st in
      res := (r, i) :: !res
    done
  with Stack.Empty -> ()
  end;
  !res
(*e: function Lexgen.map_on_all_states *)

(*s: function Lexgen.goto_state *)
let goto_state st =
  if Set.is_empty st 
  then Backtrack 
  (* can create a new state in todo *)
  else Goto (get_state st)
(*e: function Lexgen.goto_state *)

(*s: function Lexgen.transition_from *)
let transition_from charsets follow pos_set = 
  let tr    = Array.create (Ast.charset_size + 1) Set.empty in
  pos_set |> List.iter (fun pos ->
     charsets.(pos) |> List.iter (fun c ->
           tr.(c) <- Set.union tr.(c) follow.(pos)
      )
  );

  let shift = Array.create (Ast.charset_size + 1) Backtrack in
  for i = 0 to Ast.charset_size do
    shift.(i) <- goto_state tr.(i)
  done;
  shift
(*e: function Lexgen.transition_from *)

(*s: function Lexgen.translate_state *)
let translate_state charsets follow = 
 fun state ->
  match split_trans_set state with
    (n, []) -> Perform n
  | (n, ps) -> Shift((if n = no_action then No_remember else Remember n),
                     transition_from charsets follow ps)
(*e: function Lexgen.translate_state *)

(*s: function Lexgen.encode_lexentries *)
let encode_lexentries charsets lexentries =
  reset_state_mem();

  (* pass 1 on lexentries, get the initial states *)
  let automata_entries =
    lexentries |> List.map (fun entry ->
        { auto_name    = entry.lex_name;
          auto_actions = entry.lex_actions;
          (* get_state() will populate todo and state_map globals *)
          auto_initial_state = get_state (firstpos entry.lex_regexp);
        }
     )
  in
  (* pass 2 on lexentries, get the transitions *)

  let follow = followpos (Array.length charsets) lexentries in
  let states = 
    map_on_all_states (fun st -> translate_state charsets follow st) 
  in
  let transitions = Array.create !next_state_num (Perform 0) in
  states |> List.iter (fun (act, i) -> transitions.(i) <- act);
  (automata_entries, transitions)
(*e: function Lexgen.encode_lexentries *)

(*s: function Lexgen.make_dfa *)
let make_dfa lexdef =
  let (charsets, lex_entries) = 
    encode_lexdef lexdef in
  let (automata_entries, automata_transitions) = 
    encode_lexentries charsets lex_entries in
  automata_entries, automata_transitions
(*e: function Lexgen.make_dfa *)
(*e: lex/lexgen.ml *)
