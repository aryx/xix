(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reusable code across the different arch-specific code generators *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'xtra action = {
  (* a multiple of 4 *)
  size: int;
  binary: unit -> Bits.int32 list;
  x: 'xtra option;
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

(* reusable rules across archs *)
