(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reusable code across the different arch-specific code generators *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type pool =
  (* note that it is not always an int! Sometimes it can be an
   * Address which will be resolved only at the very end.
   *)
  | PoolOperand of Ast_asm.ximm
  (* todo: still don't know why we need that *)
  | LPOOL 

type action = {
  (* a multiple of 4 *)
  size: int;
  pool: pool option;
  binary: unit -> Bits.int32 list;
}

type env = {
  syms: Types.symbol_table2;
  (* for the codegen/size_of_instruction to know how to handle instructions
   * using (FP) (the frame pointer)
   *)
  autosize: int;
}
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
