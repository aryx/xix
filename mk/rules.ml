
type 'a rule_ = {
  targets: 'a list;
  prereqs: 'a list;

  attrs: Ast.rule_attribute Set.t;
  recipe: Ast.recipe option;
}


type t = {
  (* use Hashtbl.find_all since a target can be associated to multiple rules *)
  simples: (string, string rule_) Hashtbl.t;

  metas: (Percent.pattern rule_) list;
}

type rule_exec = {
  recipe2: Ast.recipe option;
  stem: string option;

  (* loc? *)
}

let rule_exec r =
  { recipe2 = r.recipe;
    stem = None
  }
