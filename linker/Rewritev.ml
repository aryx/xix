(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types
open Ast_asmv

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rewrite (cg : instr T.code_graph) : instr T.code_graph =

  let is_leaf : A.global Hashtbl_.set  = Hashtbl_.create () in

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
          | A.RET -> (curtext, Some n)
          | A.Load _ | A.Store _ | A.AddI _ | A.Cmp _
          | A.Jmp _ | A.JmpAndLink _ | A.JEq _ ->
             (curtext, Some n)
        in
        (* NOP and RET should not have branch set *)
        n.branch |> Option.iter (fun _n2 ->
          raise (Impossible "branch should not be set on virtual instr")
        );
        env
        
    | T.I (instr) ->
        let env = 
          match instr with
          | JAL _ | Bxx ((GEZAL | LTZAL), _, _) -> 
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
   * graph so RET (processed later) knows which of the 3 shapes applies.
   * None = case 1 (no frame at all); Some(autosize, false) = case 2
   * (leaf with a frame, no link save/restore); Some(autosize, true) =
   * case 3 (not leaf, full save/restore) -- mirrors Rewritei.ml's own
   * `frame` shape (RISC-V), ported here to fix a real, confirmed bug:
   * see the TEXT case's own comment below for what was wrong before. *)
  cg |> T.iter_with_env (fun (frame : (int * bool) option) n ->
    match n.instr with
    | T.TEXT (global, attrs, size) ->
        (* sanity checks *)
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (A.s_of_global global));
        if size < 0
        then failwith "TODO: handle size local -4";

        let is_leaf_here = Hashtbl.mem is_leaf global in
        let frame =
          (* TODO? in theory can do something different when size == 0 and not
           * leaf but simpler to have less cases
           *)
          if size == 0 && is_leaf_here
          then begin
             Logs.debug (fun m -> m "found a leaf procedure without locals: %s"
                          (A.s_of_global global));
             (* not needed: n.instr <- T.TEXT (global, attrs, 0) *)
             None
          end
          (* + 4 extra space for saving rLINK *)
          else Some (size + 4, not is_leaf_here)
        in
        frame |> Option.iter (fun (autosize, needs_link_save) ->
          (* for layout text we need to set the final autosize *)
          n.instr <- T.TEXT (global, attrs, autosize);
          (* claude: a leaf function that still needs a frame
           * (autosize > 0, e.g. it has locals but calls nothing)
           * still gets the SP adjustment below, but must NOT save
           * RLINK -- nothing ever calls out of it, so RLINK is
           * never clobbered, and goken's own noop.c (the ATEXT case)
           * confirms this: `autosize = p->to.offset+4;` always, but
           * the AMOVW REGLINK,0(SP) save is only emitted
           * `if(!(curtext->mark & LEAF))`. The old code here always
           * emitted both, unconditionally -- caught by
           * tests/linker/mips_diff/lacon_mips.s (a leaf function
           * with an 8192-byte frame), which had 2 extra spurious
           * words (this RLINK save) before this fix.
           *
           * The RET side (below) had the exact same latent gap for a
           * leaf function with autosize > 0 -- unconditionally doing
           * a load-from-memory+restore+jmp epilogue even though the
           * prologue here never actually stored anything at 0(SP) for
           * a leaf, meaning R2 would be loaded with garbage and
           * jumped to. Confirmed as a REAL bug (not just a
           * theoretical one), not previously caught since no MIPS
           * fixture exercised RET on a leaf-with-locals function --
           * discovered and fixed while investigating RISC-V's own
           * `riscv_port.md`/`riscv_port.md`, which
           * had flagged this exact question (goken's il/noop.c has
           * the identical 3-way shape) without checking whether it
           * also applied to MIPS's vl/noop.c; it does, confirmed by
           * reading vl/noop.c's ATEXT/ARET cases directly. Fixed by
           * threading `needs_link_save` (rather than just an autosize
           * option) through to the RET case below, same shape as
           * Rewritei.ml's own `frame : (int * bool) option`. *)
          let n1 = T.{
            instr = T.I (Arith (ADD (W, A.S),
                         Imm (- autosize), None, rSP));
            next =
              (if not needs_link_save then n.next
               else Some T.{
                 instr = T.I (Move2 (W__,
                                   Either.Left (Gen (GReg rLINK)),
                                   Gen (Indirect (rSP, 0))));
                 next = n.next;
                 branch = None; n_loc = n.n_loc; real_pc = -1;
               });
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
            n.instr <- T.I (JMP (ref (A.IndirectJump (rLINK))))
          | Some (autosize, false) ->
            (* case 2 (leaf with a frame, no link save/restore):
             * ADD $autosize, SP
             * JMP (RLINK) *)
            n.instr <- T.I (Arith (ADD (W, A.S), Imm autosize, None, rSP));
            let n1 = T.{
              instr = T.I (JMP (ref (A.IndirectJump (rLINK))));
              next = n.next;
              branch = None; n_loc = n.n_loc; real_pc = -1;
            }
            in
            n.next <- Some n1
          | Some (autosize, true) ->
            (* case 3 (not leaf, full save/restore):
             * MOVW 0(SP), R2
             * ADD $autosize, SP
             * JMP (R2)
             * alt? why not reusing rLINK instead of an extra R2?
             *)
            n.instr <- T.I (Move2 (W__,
                           Either.Left (Gen (Indirect (rSP, 0))),
                           Gen (GReg r2TMP)));

            let rec n1 = T.{
              instr = T.I (Arith (ADD (W, A.S),
                         Imm (autosize), None, rSP));
              next = Some n2;
              branch = None; n_loc = n.n_loc; real_pc = -1;
             }
            and n2 = T.{
              instr = T.I (JMP (ref (A.IndirectJump (r2TMP)))) ;
              next = n.next;
              branch = None; n_loc = n.n_loc; real_pc = -1;
          }
          in
          n.next <- Some n1;
        );

        | A.NOP -> raise (Impossible "NOP was removed in step1")

        | A.JmpAndLink opd ->
            n.instr <- T.I (JAL opd)
        | A.AddI (sign, i, reg) ->
            n.instr <- T.I (Arith (ADD (W, sign), Imm i, None, reg))
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

     | T.I  ( Arith _ | NOR _ | ArithMul _ | ArithF _
            | Move1 _ | Move2 _
            | JMP _ | RFE _ | JAL _ | JALReg _ | BEQ _ | BNE _
            | Bxx _
            | SYSCALL | BREAK | TLB _
            | LL _ | SC _
            ) ->
        frame
  ) None;

  (* works by side effect, still return first node *)
  cg


