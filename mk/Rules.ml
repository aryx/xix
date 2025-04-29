
type 'a rule = {
  targets: 'a list;
  prereqs: 'a list;

  attrs: Ast.rule_attribute Set_.t;
  recipe: Ast.recipe option;

  loc: Ast.loc;
}


type rules = {
  (* use Hashtbl.find_all because a target can be associated to many rules *)
  simples: (string, string rule) Hashtbl.t;

  metas: (Percent.pattern rule) list;
}

type rule_exec = {
  recipe2: Ast.recipe option;
  attrs2: Ast.rule_attribute Set_.t;
  loc2: Ast.loc;

  stem: string option;

  all_targets: string list;
  all_prereqs: string list;
}

(* helpers *)
let has_recipe re =
  re.recipe2 <> None

let is_meta re =
  re.stem <> None
