(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
module T = Types

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
let default_rules (_env : env) (_init_data : T.addr option) 
    (node : 'a T.node) : 'xtra action =
  match node.instr with
   | T.Virt _ -> 
      raise (Impossible "rewrite should have transformed virtual instrs")
  (* TEXT instructions were kept just for better error reporting localisation 
   * case 0: /* pseudo ops */
   *)
  | T.TEXT (_, _, _) -> 
      { size = 0; x = None; binary = (fun () -> []) }
  | T.WORD _ ->
      failwith "WORD not handled yet"
  | T.I _ ->
      raise (Impossible "codegen should not call default_rules for instr")
