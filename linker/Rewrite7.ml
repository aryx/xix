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
open Ast_asm7

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TEXT/RETURN rewrite, ported from goken's own linkers/7l/noop.c.
 *
 * claude: goken's grammar has TWO distinct return-shaped mnemonics: the
 * real hardware "RET [Rn]" (this port's own `Ast_asm7.RET`, a plain
 * instruction, never touched here) and the compiler-facing "RETURN"
 * pseudo-op (lexed to the *shared* `Ast_asm.virtual_instr.RET`, exactly
 * like ARM32/MIPS/RISC-V's own compiler-only RET) which noop.c expands
 * based on leaf/frame-size analysis -- this file only ever handles the
 * latter. Real 7c-generated code emits "RETURN", not "RET"; a
 * hand-written .s fixture wanting the automatic prologue/epilogue must
 * do the same.
 *
 * goken's algorithm (noop.c), ported as directly as possible:
 *  - leaf detection: a TEXT is a leaf unless some ABL (call)
 *    instruction appears inside it (marks &= ~LEAF at every ABL,
 *    nothing else clears it -- notably NOT affected by whether the
 *    body references the link register directly, e.g. via a raw
 *    "RET R30").
 *  - autosize = declared + PCSZ(8) (goken also special-cases a
 *    negative declared size to autosize=0, kept for parity even
 *    though this port's grammar already rejects a negative size
 *    earlier, so that branch is unreachable here).
 *  - if leaf and autosize <= PCSZ(8): autosize = 0 (no prologue at
 *    all -- case 1, matching every other arch's "leaf, no locals"
 *    case, just with PCSZ=8 instead of a flat 4).
 *  - else: round autosize up to STACKALIGN(16).
 *  - aoffset = min(autosize, 0xF0), but effectively unused for a leaf
 *    (no link-register save needed there at all, regardless of size).
 *  - prologue: for a leaf with autosize<>0, just "SUB $autosize,SP,SP"
 *    (case 2); for a non-leaf (case 3), "[SUB $(autosize-aoffset),
 *    SP,SP if autosize>aoffset]" then "STR RLINK,-aoffset(SP)!"
 *    (pre-index write-back store).
 *  - RETURN expansion: for a leaf, "[ADD $autosize,SP,SP if
 *    autosize<>0]; RET R30" (case 1/2); for a non-leaf (case 3),
 *    "LDR (SP)aoffset!,RLINK (post-index write-back load); [ADD
 *    $(autosize-aoffset),SP,SP if autosize>aoffset]; RET R30".
 *)

let pcsz = 8
let stackalign = 16

let round_up_stackalign (n : int) : int =
  if n mod stackalign = 0 then n else n + (stackalign - (n mod stackalign))

let mk_node (loc : T.loc) (instr : instr T.code_bis) : instr T.node =
  T.{ instr; next = None; branch = None; n_loc = loc; real_pc = -1 }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rewrite (cg : instr T.code_graph) : instr T.code_graph =

  let is_leaf : A.global Hashtbl_.set = Hashtbl_.create () in

  (* step1: mark is_leaf and delete NOPs, same shape as Rewrite5.ml/
   * Rewritei.ml *)
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
          match instr with
          | BL _ ->
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

  (* step2: transform, threading (autosize, is_leaf) through the graph
   * so RETURN (processed later) knows which of the 3 shapes applies.
   * None = case 1 (no frame at all); Some(autosize, true) = case 2
   * (leaf with a frame, no link save/restore); Some(autosize, false) =
   * case 3 (not leaf, full save/restore). *)
  cg |> T.iter_with_env (fun (frame : (int * bool) option) n ->
    match n.instr with
    | T.TEXT (global, attrs, size) ->
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (A.s_of_global global));
        if size < 0
        then failwith "TODO: handle size local -4";

        let leaf = Hashtbl.mem is_leaf global in
        let autosize0 = size + pcsz in
        let autosize =
          if leaf && autosize0 <= pcsz then 0
          else round_up_stackalign autosize0
        in
        let leaf = if autosize = 0 && not leaf then true else leaf in

        let frame =
          if autosize = 0 then None (* case 1 *)
          else Some (autosize, leaf) (* case 2 (leaf) or case 3 (not leaf) *)
        in
        let orig_next = n.next in
        frame |> Option.iter (fun (autosize, leaf) ->
          n.instr <- T.TEXT (global, attrs, autosize - pcsz);
          if leaf then begin
            (* case 2: SUB $autosize,SP,SP *)
            let sub_node = mk_node n.n_loc
              (T.I (Arith (SUB, Imm autosize, None, rSP)))
            in
            sub_node.next <- orig_next;
            n.next <- Some sub_node
          end else begin
            (* case 3: [SUB $(autosize-aoffset),SP,SP] ; STR
             * RLINK,-aoffset(SP)! *)
            let aoffset = min autosize 0xF0 in
            let store_node = mk_node n.n_loc
              (T.I (Move (X_, Either.Left (GReg rLINK), PreIndex (rSP, -aoffset))))
            in
            store_node.next <- orig_next;
            if autosize > aoffset then begin
              let sub_node = mk_node n.n_loc
                (T.I (Arith (SUB, Imm (autosize - aoffset), None, rSP)))
              in
              sub_node.next <- Some store_node;
              n.next <- Some sub_node
            end else
              n.next <- Some store_node
          end
        );
        frame

    | T.WORD _ -> frame
    | T.Virt virt ->
        (match virt with
        | A.RET ->
          (match frame with
          | None ->
            (* case 1: RET R30 *)
            n.instr <- T.I (RET (Some rLINK))
          | Some (autosize, true) ->
            (* case 2: ADD $autosize,SP,SP; RET R30 *)
            n.instr <- T.I (Arith (ADD, Imm autosize, None, rSP));
            let ret_node = mk_node n.n_loc (T.I (RET (Some rLINK))) in
            ret_node.next <- n.next;
            n.next <- Some ret_node
          | Some (autosize, false) ->
            (* case 3: LDR (SP)aoffset!,RLINK ; [ADD
             * $(autosize-aoffset),SP,SP] ; RET R30 *)
            let aoffset = min autosize 0xF0 in
            n.instr <- T.I (Move (X_, Either.Left (PostIndex (rSP, aoffset)), GReg rLINK));
            let ret_node = mk_node n.n_loc (T.I (RET (Some rLINK))) in
            ret_node.next <- n.next;
            if autosize > aoffset then begin
              let add_node = mk_node n.n_loc
                (T.I (Arith (ADD, Imm (autosize - aoffset), None, rSP)))
              in
              add_node.next <- Some ret_node;
              n.next <- Some add_node
            end else
              n.next <- Some ret_node
          );
          frame

        | A.NOP -> raise (Impossible "NOP was removed in step1")
        | A.JmpAndLink opd ->
            n.instr <- T.I (BL opd); frame
        | A.AddI (_sign, i, reg) ->
            n.instr <- T.I (Arith (ADD, Imm i, None, reg)); frame
        | A.Load (ent, reg) ->
            n.instr <- T.I (Move (X_, Either.Left (Entity ent), GReg reg)); frame
        | A.Store (reg, ent) ->
            n.instr <- T.I (Move (X_, Either.Left (GReg reg), Entity ent)); frame
        | A.Jmp opd ->
            n.instr <- T.I (B opd); frame
        | A.Cmp _ -> raise Todo
        | A.JEq _ -> raise Todo
       )

     | T.I (Arith _ | Shift _ | Cmp _ | ArithMul _ | Move _
           | B _ | BL _ | Bxx _ | CBxx _ | TBxx _ | RET _ | SVC _
           | FArith _ | FCmp _ | Barrier _ | CondSel _ | CondSet _
           | LoadExcl _ | StoreExcl _
           ) ->
        frame
  ) None;

  (* works by side effect, still return first node *)
  cg
