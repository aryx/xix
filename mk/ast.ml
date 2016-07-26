
type var = 
  | SimpleVar of string
  (* todo: ${name} or ${name: % = %} *)

type word_elem =
  | String of string
  | Percent
  (* the string does not include the quotes *)
  | Quoted of string

  (* evaluated during parsing *)
  | Var of var
  | Backquoted of string

(* no separator; direct concatenation *)
type word = word_elem list

(* separated by spaces *)
type words = word list


(* the strings do not contain the leading space nor trailing newline *)
type recipe = string list

type rule_ = {
  targets: words;
  prereqs: words;
  attr: rule_attribute list;
  recipe: recipe option;

  (* derived from whether targets or prereqs contain a Percent *)
  is_meta: bool;
}
  (* less: could make an enum *)
  and rule_attribute = char

type instr_kind =
  (* should resolve to a single filename *)
  | Include of words
  | Rule of rule_
  (* stricter: I disallow dynamic def like $X=42 where there was a X=AVAR *)
  | Definition of string * bool (* private *) * words

type loc = {
  file: Common.filename;
  line: int;
}

type instr = {
  instr: instr_kind;
  loc: loc;
}
