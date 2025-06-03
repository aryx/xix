(*s: Rules.ml *)

(*s: type [[Rules.rule]] *)
type 'a rule = {
  targets: 'a list;
  prereqs: 'a list;
  recipe: Ast.recipe option;
  (*s: [[Rules.rule]] other fields *)
  attrs: Ast.rule_attribute Set_.t;
  (*e: [[Rules.rule]] other fields *)
  loc: Ast.loc;
}
(*e: type [[Rules.rule]] *)


(*s: type [[Rules.rules]] *)
type rules = {
  (* use Hashtbl.find_all because a target can be associated to many rules *)
  simples: (string, string rule) Hashtbl.t;
  metas:  (Percent.pattern rule) list;
}
(*e: type [[Rules.rules]] *)

(*s: type [[Rules.rule_exec]] *)
type rule_exec = {
  recipe2: Ast.recipe option;
  stem: string option;

  all_targets: string list;
  all_prereqs: string list;
  (*s: [[Rules.rule_exec]] other fields *)
  attrs2: Ast.rule_attribute Set_.t;
  (*e: [[Rules.rule_exec]] other fields *)
  loc2: Ast.loc;
}
(*e: type [[Rules.rule_exec]] *)

(*s: function [[Rules.has_recipe]] *)
(* helpers *)
let has_recipe re =
  re.recipe2 <> None
(*e: function [[Rules.has_recipe]] *)

(*s: function [[Rules.is_meta]] *)
let is_meta re =
  re.stem <> None
(*e: function [[Rules.is_meta]] *)
(*e: Rules.ml *)
