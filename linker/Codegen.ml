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

        (* claude: the low 32 bits of a double literal's own raw
         * IEEE754 bit pattern (goken's own 8-byte pool entry for a
         * real "FMOVD $con,Fd" -- see ARM64's Codegen7.ml/Layout7.ml,
         * which splices an explicit high-word WORD node right after
         * this one for the other 4 bytes, same shape as the Int case
         * above). Found stress-testing real lib_core/libc (fmt/
         * strtod.c's real "FMOVD $4.29496729500000000e+09,F3" and
         * siblings), see docs/claude_notes/plan_hello_libc_linking.md. *)
        | Ast_asm.Float f ->
            (* claude: recent OCaml would just do:
             *   let bits = Int64.bits_of_float f in
             *   [ [(Int64.to_int (Int64.logand bits 0xFFFFFFFFL), 0)] ]
             * -- Bits_of_float.hi_lo_of_float64 gives the same low
             * 32-bit half as an Int32.t; `land 0xffffffff` recovers
             * the same unsigned `int` Int64.logand/to_int used to
             * (Int32.to_int alone would sign-extend a half with its
             * top bit set into a negative int). *)
            [ [(Int32.to_int (snd (Bits_of_float.hi_lo_of_float64 f)) land 0xffffffff, 0)] ]

        | Ast_asm.String _s -> 
            (* stricter? what does 5l do with that? confusing I think *)
            (* error node ... *)
            failwith "string not allowed with WORD; use DATA"

        (* claude: offset_from_sym used to be discarded here too (same
         * `_offsetTODO` naming as Codegen5.ml's own copy of this bug
         * -- both silently computed sym+0 for any "MOVW $sym+N(SB),RT"
         * that overflows into a literal-pool WORD instead of the fast
         * ADD-based path). This is the one actually exercised by a
         * real "$.string<>+N(SB)" reference with N large enough that
         * offset_to_R12 doesn't fit an immediate rotate -- see
         * Codegen5.ml's own fix and
         * docs/claude_notes/plan_hello_libc_linking.md. *)
        | Ast_asm.Address (Ast_asm.Global (global, offset_from_sym)) ->
            let v = Hashtbl.find env.syms (Types.symbol_of_global global) in
            (match v with
             | Types.SText2 real_pc -> [ [(real_pc + offset_from_sym, 0)] ]
             | Types.SData2 (offset, _kind) ->
                 (match init_data with
                 | None -> raise (Impossible "init_data should be set by now")
                 | Some init_data -> [ [(init_data + offset + offset_from_sym, 0)] ]
                 )
            )

        | Ast_asm.Address (Ast_asm.Param _ | Ast_asm.Local _) -> raise Todo
      )}

  | Types.I _ ->
      raise (Impossible "codegen should not call default_rules for instr")
