(*s: Rewrite5.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types
open Ast_asm5

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Rewrite5.find_first_no_nop_node]] *)
(*e: function [[Rewrite5.find_first_no_nop_node]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Rewrite5.rewrite]] *)
(* less: rewrite when profiling flag -p *)
let rewrite (cg : 'a T.code_graph) : 'a T.code_graph =
  
  let is_leaf : A.global Hashtbl_.set = Hashtbl_.create () in

  (* step1: mark is leaf and delete NOPs *)
  cg |> T.iter_with_env (fun (curtext, prev_no_nop) n ->
    match n.T.instr with
    | T.TEXT (ent, _attrs, _size) ->
        Hashtbl.add is_leaf ent true;
        (Some ent, Some n)
    | T.WORD _ -> (curtext, Some n)
    | T.Virt vinstr ->
        let env = 
          match vinstr with
          (* remove the NOP *)
          | A.NOP ->
              prev_no_nop |> Option.iter (fun prev ->
                prev.T.next <- n.T.next;
              );
              (curtext, prev_no_nop)
          | A.RET ->
              raise (Impossible
                "ARM's grammar builds CRET (Ast_asm5.instr), not this \
                 shared, condition-less Ast_asm.virtual_instr.RET")
          | A.Load _ | A.Store _ | A.AddI _ | A.Cmp _
          | A.Jmp _ | A.JmpAndLink _ | A.JEq _ ->
              (curtext, Some n)
        in
        (* NOP and RET should not have branch set *)
        n.branch |> Option.iter (fun _n2 ->
          raise (Impossible "branch should not be set on virtual instr")
        );
        env
        
    | T.I (instr, _condXXX) ->
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
  
  (* step2: transform *)
  cg |> T.iter_with_env (fun autosize_opt n ->
    match n.instr with
    | T.TEXT (global, attrs, size) ->
        (* sanity checks *)
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (A.s_of_global global));
        if size < 0 
        then failwith "TODO: handle size local -4";
        
        let autosize_opt = 
          if size == 0 && Hashtbl.mem is_leaf global
          then begin
             Logs.debug (fun m -> m "found a leaf procedure without locals: %s" 
                          (A.s_of_global global));
             None
          end
          (* + 4 extra space for saving rLINK *)
          else Some (size + 4)
        in
        autosize_opt |> Option.iter (fun autosize ->
          (* for layout text we need to set the final autosize so that
           * size_of_instruction can get passed autosize and can correctly
           * handle instructions using (FP) (the frame pointer).
           *)
          n.instr <- T.TEXT (global, attrs, autosize);
          (* decrement SP and save rLINK in one operation:
           *   MOVW.W R14, -autosize(SP) 
           *)
          let n1 = T.{
            instr = T.I (MOVE (A.Word, Some WriteAddressBase, 
                              Imsr (Reg rLINK), 
                              Indirect (rSP, -autosize)), AL);
            next = n.next;
            branch = None;
            n_loc = n.n_loc;
            real_pc = -1;
          }
          in
          n.next <- Some n1;
        );
        autosize_opt

    | T.WORD _ -> autosize_opt
    | T.Virt virt ->
      (match virt with
      | A.RET ->
          raise (Impossible
            "ARM's grammar builds CRET (Ast_asm5.instr), not this \
             shared, condition-less Ast_asm.virtual_instr.RET")
      | A.NOP -> raise (Impossible "NOP was removed in step1")
      | A.JmpAndLink opd ->
          n.instr <- T.I (BL opd, AL)
      | A.Cmp (i, reg) ->
          n.instr <- T.I (Cmp (CMP, Imm i, reg), AL)
      | A.JEq opd ->
          n.instr <- T.I (Bxx (EQ, opd), AL)
      | (A.AddI _ | A.Load _ | A.Store _ | A.Jmp _) -> raise Todo
      );
      autosize_opt

     | T.I (CRET, cond) ->
        n.instr <- T.I
          ((match autosize_opt, cond with
           (* B.cond (R14) -- validated byte-identical against goken's
            * real 5a/5l, see tests/linker/arm_diff/cret_leaf.s. *)
           | None, _ -> B (ref (A.IndirectJump rLINK))
           | Some _, AL ->
              (match autosize_opt with
               | Some autosize ->
                 (* increment SP and restore rPC in one operation:
                  *    MOVW.P autosize(SP), PC
                  *)
                 MOVE (A.Word, Some PostOffsetWrite,
                       Indirect (rSP, autosize), Imsr (Reg rPC))
               | None -> assert false)
           (* claude: a *conditional* RET in a framed (non-leaf)
            * procedure -- e.g. fmt/dofmt.c's "if(...) return ...;"
            * compiled with locals allocated. Tried the same
            * MOVW.P.cond expansion as the leaf case (single predicated
            * load-with-writeback), but a differential test against
            * goken's real 5a/5l (tests/linker/arm_diff/
            * cret_framed.s, kept as a scratch repro, not committed)
            * showed goken emits a shorter/different byte sequence --
            * genuinely not yet reverse-engineered, so fail loudly
            * instead of silently emitting bytes that don't match
            * goken, same convention as riscv_port.md's case-18 guard. *)
           | Some _, _ ->
              raise Todo
           ), cond);
        autosize_opt

     | T.I (
            ( RFE | Arith _ | ArithF _ | MOVWF _ | MOVFW _ | MOVE _ | MOVEF _
            | SWAP _ | B _ | BL _ | Cmp _ | CmpF _ | Bxx _
            | SWI _ | MULL _ | MOVM _
            )
            , _) ->
        autosize_opt
  ) None;

  (* works by side effect, still return first node *)
  cg
(*e: function [[Rewrite5.rewrite]] *)
(*e: Rewrite5.ml *)
