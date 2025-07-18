(*s: mk/Ast.ml *)
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
    (*s: [[Ast.word_element]] cases *)
     (* stricter: backquotes are allowed only in word context, not at toplevel
      * so no `echo '<foo.c'` *)
     (* `...` or `{...} (the string does not include the backquote or braces) *)
    | Backquoted of string
    (*e: [[Ast.word_element]] cases *)
(*e: type [[Ast.word_element]] *)

(*s: type [[Ast.var]] *)
and var = 
 (* $name or ${name} (the string does not contain the $ or {}) *)
| SimpleVar of string
(*s: [[Ast.var]] cases *)
 (* ${name:a%b=c%d} *)
| SubstVar of (string * word * word list)
(*e: [[Ast.var]] cases *)
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

(*s: type [[Ast.rule]] *)
(* See also Rules.rule and Rules.rule_exec *)
type rule = {
  targets: words;
  prereqs: words;
  recipe: recipe option;
  (*s: [[Ast.rule]] other fields *)
  attrs: rule_attribute list;
  (*e: [[Ast.rule]] other fields *)
}
(*e: type [[Ast.rule]] *)
(*s: type [[Ast.rule_attribute]] *)
and rule_attribute =
(*s: [[Ast.rule_attribute]] cases *)
| Virtual
(*x: [[Ast.rule_attribute]] cases *)
| Delete
(*x: [[Ast.rule_attribute]] cases *)
| Quiet
(*x: [[Ast.rule_attribute]] cases *)
| Interactive (* pad: I added this one *)
(*e: [[Ast.rule_attribute]] cases *)
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
    | Rule of rule
    (* should resolve to a single filename less: could enforce of word? *)
    | Include of words
    (* stricter: no dynamic def like X=AVAR  $X=42 ... $AVAR, 
     * so 'string' below, not 'word' *)
    | Definition of string * words
    (*s: [[Ast.instr_kind]] cases *)
    (* the words can contain variables, ex: <|rc ../foo.rc $CONF 
     * less: we could also do PipeInclude of recipe I think *)
    | PipeInclude of words
    (*e: [[Ast.instr_kind]] cases *)
(*e: type [[Ast.instr_kind]] *)
[@@deriving show {with_path = false}]

(* for boostrap-mk.sh and ocaml-light to work without deriving *)
let show_instrs _ = "NO DERIVING"
[@@warning "-32"]

(*s: type [[Ast.instrs]] *)
type instrs = instr list
(*e: type [[Ast.instrs]] *)
[@@deriving show]

(*s: function [[Ast.dump_ast]] *)
let dump_ast instrs =
  Logs.app (fun m -> m "AST = %s" (show_instrs instrs))
(*e: function [[Ast.dump_ast]] *)
(*e: mk/Ast.ml *)
