(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(* module T = Types but then ocaml-light issue in Codegen5.ml :( WEIRD *)

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
let default_rules (env : env) (init_data : Types.addr option) 
    (node : 'a Types.node) : 'xtra action =
  match node.instr with
   | Types.Virt _ -> 
      raise (Impossible "rewrite should have transformed virtual instrs")
  (* TEXT instructions were kept just for better error reporting localisation 
   * case 0: /* pseudo ops */
   *)
  | Types.TEXT (_, _, _) -> 
      { size = 0; x = None; binary = (fun () -> []) }

  (* 5l: case 11: /* word */ *)
  (* claude: goken's asmout() just does `o1 = instoffset` here (the
   * resolved constant/address value), same as below; AWORD $lext is
   * never hand-written, it's what 5a itself generates for a
   * literal-pool entry (see optab.c's comment on the AWORD/C_LEXT
   * row: "the constant-pool word for 'MOVW $fmtalloc(SB), Rx'"), so
   * this is exercised indirectly by every fixture whose codegen
   * overflows an immediate and falls back to a literal pool (e.g.
   * Codegen5.ml's cases 9/12/13), not by a fixture that writes WORD
   * directly. *)
  | Types.WORD x ->
      { size = 4; x = None; binary = (fun () ->
        match x with
        (* TODO? should apply endianess ? *)
        | Ast_asm.Int i -> [ [(i land 0xffffffff, 0)] ]

        | Ast_asm.Float _ -> raise Todo

        | Ast_asm.String _s -> 
            (* stricter? what does 5l do with that? confusing I think *)
            (* error node ... *)
            failwith "string not allowed with WORD; use DATA"

        | Ast_asm.Address (Ast_asm.Global (global, _offsetTODO)) -> 
            let v = Hashtbl.find env.syms (Types.symbol_of_global global) in
            (match v with
             | Types.SText2 real_pc -> [ [(real_pc, 0)] ]
             | Types.SData2 (offset, _kind) -> 
                 (match init_data with
                 | None -> raise (Impossible "init_data should be set by now")
                 | Some init_data -> [ [(init_data + offset, 0)] ]
                 )
            )

        | Ast_asm.Address (Ast_asm.Param _ | Ast_asm.Local _) -> raise Todo
      )}

  | Types.I _ ->
      raise (Impossible "codegen should not call default_rules for instr")
