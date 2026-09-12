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
module T = Types
open Ast_asmi

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly TEXT/RET rewrite depending on whether a function is a "leaf",
 * ported from goken's il/noop.c (the ATEXT and ARET cases).
 *
 * claude: unlike Rewritev.ml (MIPS), which only distinguishes "leaf
 * with no locals" (no prologue at all) from everything else (always
 * saves/restores the link register even for a leaf procedure that
 * merely has locals but makes no calls), goken's il/noop.c genuinely
 * has THREE cases, and getting this right matters for byte-equality:
 *  1. leaf, no locals (autosize=0): no prologue/epilogue at all --
 *     RET is just `JMP (RLINK)`.
 *  2. leaf, with locals (autosize = declared_size + ptrsize): the
 *     prologue only adjusts SP (`ADD $-autosize,SP`); since nothing
 *     ever saved RLINK to the stack (no calls were made, so RLINK
 *     was never clobbered), the epilogue doesn't restore it either,
 *     just `ADD $autosize,SP; JMP (RLINK)`.
 *  3. not leaf (calls something, so RLINK gets clobbered by that
 *     call and must be saved first): autosize = declared_size +
 *     ptrsize (or forced to exactly 8 when declared_size was 0 --
 *     enough room to spill RLINK even with no real locals; see
 *     `forced_size_when_declared_zero` below for why that's a flat
 *     8 on both riscv32 and riscv64, not "2*ptrsize"). Prologue
 *     is `ADD $-autosize,SP` then `MOVW RLINK,0(SP)`; epilogue is
 *     `MOVW 0(SP),RLINK; ADD $autosize,SP; JMP (RLINK)`.
 * RISC-V has no branch-delay slots (unlike MIPS), which is the one
 * genuine simplification here relative to Rewritev.ml.
 *)

(* claude: goken's obj.c sets a global `ptrsize` from thechar at
 * startup -- 4 on riscv32, but *8* on riscv64 (RLINK is a full
 * 8-byte pointer there, matching case 6/7's SD/LD choice in
 * Codegeni.ml). This is a different, arch-dependent quantity from
 * the *literal* `4` used below in obj.c's own frame-size padding
 * step, which is fixed regardless of arch. Conflating the two (i.e.
 * using a single flat `4` everywhere, as this port initially did)
 * still produces a valid, functionally-correct frame on riscv32, but
 * an undersized-by-4 one on riscv64 -- caught by
 * tests/linker/riscv64_diff/case6_7_riscv64.s. *)
let ptrsize (is_64 : bool) = if is_64 then 8 else 4

(* claude: goken's obj.c (ATEXT handling) pads a nonzero declared
 * frame size before noop.c ever sees it: rounded up to a multiple of
 * 4 (a no-op here since Rewritei already requires that), then bumped
 * by one more *literal 4* (not `ptrsize (is_64)` -- this step is the
 * same fixed size on both riscv32 and riscv64) *if the rounded
 * size's bit 2 has the "wrong" parity* -- the parity check itself is
 * flipped between riscv32 and riscv64 (`thechar == 'j'`). This is
 * what actually produces goken's displayed "TEXT ...,$20" for a
 * declared `$16` frame on riscv32 (16 has bit2==0, so +4) vs. "TEXT
 * ...,$16" for the same declared size on riscv64 (bit2==0 there
 * means *no* bump, since riscv64's check is inverted). Then noop.c's
 * own ATEXT case adds a *second*, unconditional `ptrsize (is_64)` on
 * top of *that* to get the real SP-adjust amount (span.c's case-1
 * `autosize = p->to.offset + ptrsize`) -- see the `frame` computation
 * below for where that second addition happens. Missing this padding
 * step entirely (as this port initially did) still produces a valid,
 * functionally-correct frame, just the wrong *size* of one whenever
 * the declared size's bit 2 has the "wrong" parity for the arch --
 * caught by tests/linker/riscv_diff/case6_7_riscv.s, the first
 * fixture to use a nonzero, non-multiple-of-8 frame size on a leaf
 * function. *)
let padded_declared_size (is_64 : bool) (size : int) : int =
  if size = 0 then 0
  else if is_64
  then (if size land 4 <> 0 then size + 4 else size)
  else (if size land 4 = 0 then size + 4 else size)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rewrite (is_64 : bool) (cg : instr T.code_graph) : instr T.code_graph =

  let is_leaf : A.global Hashtbl_.set = Hashtbl_.create () in

  (* step1: mark is_leaf and delete NOPs, same shape as Rewritev.ml *)
  cg |> T.iter_with_env (fun (curtext, prev_no_nop) n ->
    match n.T.instr with
    | T.TEXT (ent, _attrs, _size) ->
        Hashtbl.add is_leaf ent true;
        (Some ent, Some n)
    | T.WORD _ -> (curtext, Some n)
    | T.Virt vinstr ->
        let env =
          match vinstr with
          | A.NOP ->
              prev_no_nop |> Option.iter (fun prev ->
                prev.T.next <- n.T.next;
              );
              (curtext, prev_no_nop)
          | A.RET -> (curtext, Some n)
          | A.Load _ | A.Store _ | A.AddI _ | A.Cmp _
          | A.Jmp _ | A.JmpAndLink _ | A.JEq _ ->
             (curtext, Some n)
        in
        n.branch |> Option.iter (fun _n2 ->
          raise (Impossible "branch should not be set on virtual instr")
        );
        env

    | T.I instr ->
        let env =
          (* a JAL/JALR is a call -- clobbers RLINK, so not a leaf.
           * JALRI (case 5, "jalr D,I(S)"/"jmp I(S)") is the same
           * AJAL-vs-AJMP mnemonic aliasing at the AST level (see
           * Ast_asmi.ml's own comment) -- goken's noop.c only clears
           * LEAF for the AJAL identity, never AJMP (a true jump,
           * never saving a link), and since this port's grammar only
           * ever produces a JALRI with rd<>rZERO for the "JALR
           * D,I(S)" spelling (rZERO is always the JMP-spelled
           * default, never user-overridable through the grammar),
           * checking rd<>rZERO here is the exact same distinction. *)
          match instr with
          | JAL _ | JALR _ ->
              curtext |> Option.iter (fun p -> Hashtbl.remove is_leaf p);
              (curtext, Some n)
          | JALRI (rd, _, _) when rd <> rZERO ->
              curtext |> Option.iter (fun p -> Hashtbl.remove is_leaf p);
              (curtext, Some n)
          | _ -> (curtext, Some n)
        in
        n.branch |> Option.iter (fun (n2 : 'a T.node) ->
          match n2.instr with
          | T.Virt A.NOP -> n.branch <- Rewrite.find_first_no_nop_node n2.next
          | _ -> ()
        );
        env
  ) (None, None);

  (* step2: transform, threading (autosize, needs_link_save) through the
   * graph so RET (processed later) knows which of the 3 cases applies.
   * None = case 1 (no frame at all); Some(autosize, false) = case 2
   * (leaf with locals, no link save/restore); Some(autosize, true) =
   * case 3 (not leaf, full save/restore).
   *)
  cg |> T.iter_with_env (fun (frame : (int * bool) option) n ->
    match n.instr with
    | T.TEXT (global, attrs, size) ->
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (A.s_of_global global));
        if size < 0
        then failwith "TODO: handle size local -4";

        let leaf = Hashtbl.mem is_leaf global in
        let padded_size = padded_declared_size is_64 size in
        (* claude: goken's noop.c forces this size-0-non-leaf case to
         * exactly 8, via a branch that only fires `else if(ptrsize
         * == 4)` -- i.e. the "bump to 2*ptrsize" trick is a riscv32-
         * only workaround (2*4=8) for autosize otherwise coming out
         * too small; on riscv64, ptrsize is already 8, so autosize
         * (= 0 + ptrsize) is *already* 8 with no bump needed at all.
         * Both arches land on 8 -- NOT `2 * ptrsize is_64`, which
         * would double-count riscv64's already-8-byte ptrsize into
         * 16 (a regression caught by tests/linker/riscv64_diff/
         * case4_riscv64.s, which uses exactly this shape: a
         * non-leaf JAL-calling function with a declared $0 frame). *)
        let forced_size_when_declared_zero =
          (match ptrsize is_64 with 4 -> 8 | pr -> pr) in
        let frame =
          if size = 0 && leaf
          then None (* case 1 *)
          else if leaf
          then Some (padded_size + ptrsize is_64, false) (* case 2 *)
          else Some ((if size = 0
                      then forced_size_when_declared_zero
                      else padded_size + ptrsize is_64), true) (* case 3 *)
        in
        frame |> Option.iter (fun (autosize, needs_link_save) ->
          (* claude: the TEXT pseudo's own stored size is
           * `padded_size`, NOT the SP-adjust amount `autosize` above
           * (which folds in one more ptrsize on top) -- matches
           * goken's own `p->to.offset` (post-obj.c-padding,
           * pre-noop.c's separate +ptrsize for the actual
           * instruction). Future Local/Param-relative addressing
           * (case 12-16, not yet implemented) must add ptrsize itself
           * at the point of computing an offset, same as goken's
           * span.c `instoffset = autosize + a->offset + ptrsize` --
           * don't bake it in here too, or it'd be double-counted. *)
          n.instr <- T.TEXT (global, attrs, padded_size);
          (* ADD $-autosize, SP
           * [MOVW RLINK, 0(SP)]   -- only for case 3
           *)
          let rec n1 = T.{
            instr = T.I (Arith (ADD None, Imm (- autosize), None, rSP));
            next = (if needs_link_save then Some n2 else n.next);
            branch = None; n_loc = n.n_loc; real_pc = -1;
          }
          and n2 = T.{
            instr = T.I (Move2 (W__,
                              Either.Left (Gen (GReg rLINK)),
                              Gen (Indirect (rSP, 0))));
            next = n.next;
            branch = None; n_loc = n.n_loc; real_pc = -1;
          }
          in
          n.next <- Some n1;
        );
        frame

    | T.WORD _ -> frame
    | T.Virt virt ->
        (match virt with
        | A.RET ->
          (match frame with
          | None ->
            (* case 1: JMP (RLINK) *)
            n.instr <- T.I (JMP (ref (A.IndirectJump rLINK)))
          | Some (autosize, false) ->
            (* case 2: ADD $autosize, SP; JMP (RLINK) *)
            let rec n1 = T.{
              instr = T.I (Arith (ADD None, Imm autosize, None, rSP));
              next = Some n2;
              branch = None; n_loc = n.n_loc; real_pc = -1;
             }
            and n2 = T.{
              instr = T.I (JMP (ref (A.IndirectJump rLINK)));
              next = n.next;
              branch = None; n_loc = n.n_loc; real_pc = -1;
            }
            in
            n.next <- Some n1
          | Some (autosize, true) ->
            (* case 3: MOVW 0(SP), RLINK; ADD $autosize, SP; JMP (RLINK) *)
            n.instr <- T.I (Move2 (W__,
                           Either.Left (Gen (Indirect (rSP, 0))),
                           Gen (GReg rLINK)));

            let rec n1 = T.{
              instr = T.I (Arith (ADD None, Imm autosize, None, rSP));
              next = Some n2;
              branch = None; n_loc = n.n_loc; real_pc = -1;
             }
            and n2 = T.{
              instr = T.I (JMP (ref (A.IndirectJump rLINK)));
              next = n.next;
              branch = None; n_loc = n.n_loc; real_pc = -1;
          }
          in
          n.next <- Some n1;
        );

        | A.NOP -> raise (Impossible "NOP was removed in step1")

        | A.JmpAndLink opd ->
            n.instr <- T.I (JAL opd)
        | A.AddI (_sign, i, reg) ->
            (* RISC-V's ADD (unlike MIPS's) carries no sign variant --
             * base-ISA add never traps on overflow either way *)
            n.instr <- T.I (Arith (ADD None, Imm i, None, reg))
        | A.Load (ent, reg) ->
            n.instr <- T.I (Move2 (W__, Either.Left (Gen (Entity ent)),
                                        Gen (GReg reg)))
        | A.Store (reg, ent) ->
            n.instr <- T.I (Move2 (W__, Either.Left (Gen (GReg reg)),
                                        (Gen (Entity ent))))
        | A.Jmp opd ->
            n.instr <- T.I (JMP opd)
        | A.Cmp _ -> raise Todo
        | A.JEq _ -> raise Todo
       );
       frame

     | T.I (Arith _ | ArithMul _ | ArithF _ | LUI _
           | Move1 _ | Move2 _ | FENCE_I
           | JMP _ | JAL _ | JALR _ | JALRI _ | Bxx _
           | ECALL | BREAK | SYS | CSR _
           ) ->
        frame
  ) None;

  (* works by side effect, still return first node *)
  cg
