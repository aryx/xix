(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module A = Ast_asm
module A6 = Ast_asm6
module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* claude: this pass used to be a real no-op (see git history for the
 * original comment on why: no virtual-instruction expansion needed on
 * this arch -- RET is a real hardware instruction, unlike every RISC
 * arch ported so far -- still true). It now does two real things,
 * both found stress-testing a genuine hello_libc closure against real
 * lib_core/libc, neither exercised by this port's own earlier,
 * hand-written hello_linux_amd64.s fixture:
 *  - `add_prologue_epilogue` (below) synthesizes the SP adjustment
 *    every real 6c-compiled function with locals needs -- see its own
 *    comment for why this port's earlier "no synthesis needed, it's
 *    always explicit in the source" assumption was wrong.
 *  - eliminates every `XFloatImm` (a literal float source to
 *    MOVSD/MOVSS/ADDSD/etc., e.g. real fmt/fltfmt.c's own "MOVSD
 *    $(1.0e+00),X0") by synthesizing a hidden DATA global for the
 *    constant and rewriting the instruction to reference it instead --
 *    mirroring goken's own linkers/6l/obj.c preprocessing (its
 *    AMOVSD/AMULSD/etc. switch case: "if from.type == D_FCONST,
 *    synthesize a symbol named by the float's own IEEE754 bits, emit
 *    a DATA definition for it once, rewrite the operand to reference
 *    it").
 *
 * `rewrite`'s own `code_graph -> code_graph * T.data list` shape
 * matches Profile.rewrite (see linker/Profile.ml) -- that's the
 * established way for a rewrite pass to hand new globals to the
 * linker's own data-segment layout (Layout.layout_data), which runs
 * right after this pass, before Codegen6.ml. Also takes `syms` (like
 * Profile.rewrite) to register each synthesized global's size in the
 * symbol table directly, the same way a real `GLOBL` line would via
 * Load.ml. *)

(* claude: real amd64 TEXT's own frame-size operand ("$32" in
 * "TEXT fmtfdinit+0(SB),0,$32") isn't just documentation -- goken's
 * real 6a *automatically* synthesizes a "SUB $autosize,SP" right after
 * TEXT and an "ADD $autosize,SP" right before every RET in that
 * function (confirmed the hard way: real 6c -S output for a genuine C
 * function with locals, e.g. fmt/fmtfd.c's own fmtfdinit, never
 * contains an explicit SUBQ/ADDQ at all -- yet goken's own 6a/6l
 * assembles/links it into a binary that DOES have one, "sub $0x20,%rsp"
 * right after the function's own entry point -- this port's earlier
 * assumption, that every real stack adjustment on this arch is always
 * an explicit source-level instruction, held for hello_linux_amd64.s
 * (hand-written, autosize=0 everywhere) but was never actually tested
 * against a real 6c-compiled function with locals until this session's
 * hello_libc closure hit one, crashing at runtime with a corrupted
 * frame -- not a codegen-time Todo/Impossible, since every individual
 * instruction's own encoding was already correct, just missing its
 * enclosing function's own SP adjustment entirely). Same idea as every
 * RISC arch's own Rewrite*.ml prologue/epilogue synthesis (e.g.
 * Rewrite5.ml's own step2), just simpler: amd64's CALL/RET already
 * push/pop the return address in hardware, so there's no link-register
 * save/restore to synthesize, only the SP adjustment itself, and
 * (unlike ARM's own RET, a virtual instruction folded into one real
 * "restore+return" MOVW) amd64's RET is already a real, un-rewritable-
 * in-place instruction, so the ADD is inserted as its own separate
 * node right before each one instead of replacing it. *)
let add_prologue_epilogue (cg : A6.instr T.code_graph) : unit =
  let sp_adjust (op : A6.arith_opcode) (autosize : A.offset) : A6.instr =
    A6.Arith (A6.Q_, op, A6.Imm autosize, A6.GReg A6.rSP)
  in
  (* claude: `skip_next` -- true for exactly one node right after
   * splicing in a synthesized trailing `Ret` (see the `T.I A6.Ret`
   * case below): without it, `T.iter_with_env` immediately visits
   * that same synthesized `Ret` next (it re-reads `n.next` right after
   * this callback returns -- see Layout5.ml's own comment on this),
   * which is *also* `T.I A6.Ret` and would otherwise get ANOTHER
   * epilogue spliced in front of it, forever (confirmed the hard way:
   * an actual infinite loop, hundreds of thousands of nodes, on a
   * real 35-file closure). *)
  cg |> T.iter_with_env (fun (autosize_opt, skip_next) n ->
      match n.T.instr with
      | T.TEXT (_, _, autosize) ->
          if autosize > 0 then begin
            let sub_node = T.{
              instr = T.I (sp_adjust A6.SUB autosize);
              next = n.next;
              branch = None;
              n_loc = n.n_loc;
              real_pc = -1;
            } in
            n.T.next <- Some sub_node
          end;
          (Some autosize, false)
      | T.I A6.Ret when not skip_next ->
          (match autosize_opt with
          | None | Some 0 -> (autosize_opt, false)
          | Some autosize ->
              let ret_node = T.{
                instr = T.I A6.Ret;
                next = n.next;
                branch = None;
                n_loc = n.n_loc;
                real_pc = -1;
              } in
              n.T.instr <- T.I (sp_adjust A6.ADD autosize);
              n.T.next <- Some ret_node;
              (autosize_opt, true)
          )
      | T.I A6.Ret (* skip_next *) -> (autosize_opt, false)
      | T.I _ | T.WORD _ | T.Virt _ -> (autosize_opt, false)
  ) (None, false)
  |> ignore

let synthetic_global_name (f : float) : string =
  Printf.sprintf "$f64.%Lx" (Int64.bits_of_float f)

let rewrite (syms : T.symbol_table) (cg : A6.instr T.code_graph) : A6.instr T.code_graph * T.data list =
  add_prologue_epilogue cg;
  let data = ref [] in
  let globals : (float, A.global) Hashtbl.t = Hashtbl.create 16 in
  let global_for_float (f : float) : A.global =
    match Hashtbl.find_opt globals f with
    | Some g -> g
    | None ->
        let g : A.global = { A.name = synthetic_global_name f; priv = None; signature = None } in
        Hashtbl.add globals f g;
        let v : T.value = T.lookup_global g syms in
        (match v.T.section with
        | T.SXref -> v.T.section <- T.SData 8
        | _ -> failwith (Printf.sprintf "redefinition of %s" g.A.name)
        );
        data |> Stack_.push (T.DATA (g, 0, 8 (* size *), A.Float f));
        g
  in
  let rewrite_xgen (x : A6.xgen) : A6.xgen =
    match x with
    | A6.XFloatImm f -> A6.XEntity (A.Global (global_for_float f, 0))
    | A6.XReg _ | A6.XIndirect _ | A6.XEntity _
    | A6.XIndirectScaled _ | A6.XEntityScaled _
    | A6.XLocalSP _ | A6.XLocalSPScaled _ -> x
  in
  cg |> T.iter (fun n ->
      match n.T.instr with
      | T.I (A6.MovF (prec, src, dst)) ->
          n.T.instr <- T.I (A6.MovF (prec, rewrite_xgen src, rewrite_xgen dst))
      | T.I (A6.ArithF (op, prec, src, dst)) ->
          n.T.instr <- T.I (A6.ArithF (op, prec, rewrite_xgen src, dst))
      | T.I (A6.CmpF (prec, src, dst)) ->
          n.T.instr <- T.I (A6.CmpF (prec, rewrite_xgen src, dst))
      | T.I (A6.CvtFToInt (int_width, prec, src, dst)) ->
          n.T.instr <- T.I (A6.CvtFToInt (int_width, prec, rewrite_xgen src, dst))
      | T.I (A6.CvtFPrec (src_prec, src, dst)) ->
          n.T.instr <- T.I (A6.CvtFPrec (src_prec, rewrite_xgen src, dst))
      | T.I _ | T.TEXT _ | T.WORD _ | T.Virt _ -> ()
  );
  cg, List.rev !data
