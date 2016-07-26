
type var = 
  (* $name or ${name} (the string does not contain the dollar or braces) *)
  | SimpleVar of string
  (* ${name:a%b=c%d} *)
  | SubstVar of (string * word * word)

and word_elem =
  | String of string
  | Percent
  (* evaluated during parsing *)
  | Var of var
   (* `...` or `{...} (the string does not include the backquote or braces) *)
  | Backquoted of string

(* no separator; direct concatenation *)
and word = word_elem list

(* separated by spaces *)
type words = word list


(* (the strings do not contain the leading space nor trailing newline) *)
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
  and rule_attribute = 
    | Quiet
    | Virtual
    | Delete

    (* less: not sure I want to handle them *)
    | NotHandled of char

type instr_kind =
  (* should resolve to a single filename *)
  | Include of words
  | Rule of rule_
  (* stricter: I disallow dynamic def like $X=42 where there was a X=AVAR *)
  | Definition of string * words

type loc = {
  file: Common.filename;
  line: int;
}

type instr = {
  instr: instr_kind;
  loc: loc;
}
