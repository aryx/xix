(*s: Arch_compiler.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(* operand *)
type opd = 
 { opd: operand_kind;
   typ: Type.t;
   loc: Ast.loc;
 }
and operand_kind =
 | ConstI of Ast_asm.integer
 | Register of Ast_asm.register

 (* indirect *)
 | Name of Ast.fullname * Ast_asm.offset
 | Indirect of Ast_asm.register * Ast_asm.offset

 (* was not "addressable" in original 5c, but I think it should *)
 | Addr of Ast.fullname

(*s: type [[Arch_compiler.env]] *)
type env = {
  (* same than Typecheck.typed_program.structs? *)
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
}
(*e: type [[Arch_compiler.env]] *)
(*s: type [[Arch_compiler.t]] *)
type 'instr t = {
  width_of_type: env -> Type.t -> int;
  (* really a (Ast_asm.register, bool) Hashtbl.t with the bool set for
   * reserved registers (e.g., rPC, rLINK, rSP (but not rRET which is ok))
   *)
  regs_initial: int array;
  rSP: Ast_asm.register;
  rRET: Ast_asm.register;

  arith_instr_of_op: 
    Ast.binaryOp -> Ast_asm.register -> Ast_asm.register -> Ast_asm.register ->
    'instr;
}
(*e: type [[Arch_compiler.t]] *)
(*e: Arch_compiler.ml *)
