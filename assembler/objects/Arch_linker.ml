(*s: Arch_linker.ml *)
open Common
module A = Ast_asm

(* Arch-specific methods allowing to factorize code in the linker
 * (e.g., in Load.ml)
 * alt: use a functor, but can't with ocaml-light and records are fine! 
 *)
(*s: type [[Arch_linker.t]] *)
type 'instr t = {
  branch_opd_of_instr: 'instr -> A.branch_operand option;
  visit_globals_instr: (A.global -> unit) -> 'instr -> unit;
  rTMP : A.register;
}
(*e: type [[Arch_linker.t]] *)

(* Obj.magic!! Take care!! *)
let of_arch (arch : Arch.t) : 'instr t =
  match arch with
  | Arch.Arm -> 
     (* nosemgrep: do-not-use-obj-magic *)
     Obj.magic {
       branch_opd_of_instr = Ast_asm5.branch_opd_of_instr;
       visit_globals_instr = Ast_asm5.visit_globals_instr;
       rTMP = Ast_asm5.rTMP;
     }
  | Arch.Mips -> 
     (* nosemgrep: do-not-use-obj-magic *)
     Obj.magic {
       branch_opd_of_instr = Ast_asmv.branch_opd_of_instr;
       visit_globals_instr = Ast_asmv.visit_globals_instr;
       rTMP = Ast_asmv.rTMP;
     }
  | _ -> failwith (spf "arch not supported yet: %s" (Arch.thestring arch))

(*e: Arch_linker.ml *)
