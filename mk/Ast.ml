(*s: Ast.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*s: type [[Ast.word]] *)
(* The elements below are not separated by any separator; they are direct
 * concatenations of elements (hence the need for ${name} below).
 * The list must contain at least one element.
 * ex: '%.c' -> W [Percent; String ".c"]
 *)
type word = W of word_element list
(*e: type [[Ast.word]] *)

(*s: type [[Ast.word_element]] *)
  and word_element =
    | String of string (* except the empty string *)
    | Percent
  
    (* evaluated in eval.ml just after parsing *)
    | Var of var
     (* stricter: backquotes are allowed only in word context, not at toplevel
      * so no `echo '<foo.c'` *)
     (* `...` or `{...} (the string does not include the backquote or braces) *)
    | Backquoted of string
(*e: type [[Ast.word_element]] *)

(*s: type [[Ast.var]] *)
     and var = 
      (* $name or ${name} (the string does not contain the $ or {}) *)
      | SimpleVar of string
      (* ${name:a%b=c%d} *)
      | SubstVar of (string * word * word list)
(*e: type [[Ast.var]] *)
[@@deriving show  {with_path = false}]

(*s: type [[Ast.words]] *)
(* Words are separated by spaces. See also Env.values *)
type words = word list
(*e: type [[Ast.words]] *)
[@@deriving show]


(*s: type [[Ast.recipe]] *)
(* (the strings do not contain the leading space nor trailing newline) *)
type recipe = R of string list
(*e: type [[Ast.recipe]] *)
[@@deriving show {with_path = false}]

(* See also Rules.rule_exec *)
(*s: type [[Ast.rule]] *)
type rule = {
  targets: words;
  prereqs: words;
  attrs: rule_attribute list;
  recipe: recipe option;
}
(*e: type [[Ast.rule]] *)
(*s: type [[Ast.rule_attribute]] *)
  and rule_attribute = 
    | Virtual
    | Quiet
    | Delete
    | Interactive (* pad: I added this one *)
    | NotHandled of char
(*e: type [[Ast.rule_attribute]] *)
[@@deriving show {with_path = false}]

(*s: type [[Ast.loc]] *)
(* for error reporting *)
type loc = {
  file: Fpath.t; (* an mkfile *)
  line: int;
}
(*e: type [[Ast.loc]] *)
[@@deriving show {with_path = false}]

(*s: type [[Ast.instr]] *)
type instr = {
  instr: instr_kind;
  loc: loc;
}
(*e: type [[Ast.instr]] *)

(*s: type [[Ast.instr_kind]] *)
  and instr_kind =
    (* should resolve to a single filename
     * less: could enforce of word? *)
    | Include of words
    (* the words can contain variables, ex: <|rc ../foo.rc $CONF 
     * less: we could also do PipeInclude of recipe I think *)
    | PipeInclude of words

    | Rule of rule

    (* stricter: no dynamic def like X=AVAR  $X=42 ... $AVAR, 
     * so 'string' below, not 'word' *)
    | Definition of string * words
(*e: type [[Ast.instr_kind]] *)
[@@deriving show {with_path = false}]

(*s: function [[Ast.show_instrs]] *)
(* for boostrap-mk.sh and ocaml-light to work without deriving *)
let show_instrs _ = "NO DERIVING"
[@@warning "-32"]
(*e: function [[Ast.show_instrs]] *)

(*s: type [[Ast.instrs]] *)
type instrs = instr list
(*e: type [[Ast.instrs]] *)
[@@deriving show]

(*s: function [[Ast.dump_ast]] *)
let dump_ast instrs =
  Logs.app (fun m -> m "AST = %s" (show_instrs instrs))
(*e: function [[Ast.dump_ast]] *)
(*e: Ast.ml *)
