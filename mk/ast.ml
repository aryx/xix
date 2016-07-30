(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

type var = 
  (* $name or ${name} (the string does not contain the dollar or braces) *)
  | SimpleVar of string
  (* ${name:a%b=c%d} *)
  | SubstVar of (string * word * word)

and word_element =
  | String of string
  | Percent

  (* evaluated in eval.ml just after parsing *)
  | Var of var
   (* `...` or `{...} (the string does not include the backquote or braces) *)
  | Backquoted of string

(* no separator, direct concatenation (hence the need for ${name}) *)
and word = W of word_element list

(* separated by spaces *)
type words = word list


(* (the strings do not contain the leading space nor trailing newline) *)
type recipe = R of string list

type rule_ = {
  targets: words;
  prereqs: words;
  attrs: rule_attribute list;
  recipe: recipe option;
}
  and rule_attribute = 
    | Quiet
    | Virtual
    | Delete
    | NotHandled of char

type instr_kind =
  (* should resolve to a single filename, less: could enforce of word? *)
  | Include of words
  | Rule of rule_
  (* stricter: no dynamic def like X=AVAR  $X=42 ... $AVAR *)
  | Definition of string * words

type loc = {
  file: Common.filename;
  line: int;
}

type instr = {
  instr: instr_kind;
  loc: loc;
}
