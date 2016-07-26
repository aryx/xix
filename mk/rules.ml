
type 'a rule_ = {
  targets: 'a list;
  prereqs: 'a list;

  attr: Ast.rule_attribute Set.t;
  recipe: Ast.recipe option;
}

type pattern_elem =
  | PStr of string
  | PPercent
type pattern = pattern_elem list



type t = {
  simples: (string, string rule_) Hashtbl.t;
  metas: (pattern rule_) list;
}

type rule_exec = {
  recipe: Ast.recipe option;
  stem: string option;

  (* loc? *)
}

