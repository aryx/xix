module A = Ast_asm

(* Arch-specific methods allowing to factorize code in the linker
 * (e.g., in Load.ml)
 * alt: use a functor, but can't with ocaml-light and records are fine! 
 *)
type 'instr t = {
  branch_opd_of_instr: 'instr -> A.branch_operand option;
  visit_globals_instr: (A.global -> unit) -> 'instr -> unit;
}
