(*s: Rewrite5.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module A5 = Ast_asm5
open Types
module T = Types
module T5 = Types5

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
let rewrite (cg : T5.code_graph) : T5.code_graph =
  
  let is_leaf : A.global Hashtbl_.set = Hashtbl_.create () in

  (* step1: mark is leaf and delete NOPs *)
  cg |> T.iter_with_env (fun (curtext, prev_no_nop) n ->
    match n.T.instr with
    | T.TEXT (ent, _attrs, _size) ->
        Hashtbl.add is_leaf ent true;
        (Some ent, Some n)
    | T.WORD _ -> (curtext, Some n)
    | T.V vinstr ->
        let env = 
          match vinstr with
          (* remove the NOP *)
          | A.NOP ->
              prev_no_nop |> Option.iter (fun prev ->
                prev.T.next <- n.T.next;
              );
              (curtext, prev_no_nop)
          | A.RET -> (curtext, Some n)
        in
        (* NOP and RET should not have branch set *)
        n.branch |> Option.iter (fun _n2 ->
          raise (Impossible "branch should not be set on virtual instr")
        );
        env
        
    | T.I (instr, _condXXX) ->
        let env = 
          match instr with
          | A5.BL _ -> 
              curtext |> Option.iter (fun p -> Hashtbl.remove is_leaf p);
              (curtext, Some n)
          | _ -> (curtext, Some n)
        in
        n.branch |> Option.iter (fun n2 ->
          match n2.instr with
          | T.V A.NOP -> n.branch <- Rewrite.find_first_no_nop_node n2.next 
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
          let n1 = {
            instr = T.I (A5.MOVE (A.Word, Some A5.WriteAddressBase, 
                              A5.Imsr (A5.Reg A5.rLINK), 
                              A5.Indirect (A5.rSP, -autosize)), A5.AL);
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
    | T.V A.RET ->
        n.instr <- T.I
          ((match autosize_opt with
           (* B (R14) *)
           | None -> A5.B (ref (A.IndirectJump (A5.rLINK)))
           (* increment SP and restore rPC in one operation:
            *    MOVW.P autosize(SP), PC 
            *)
           | Some autosize -> A5.MOVE (A.Word, Some A5.PostOffsetWrite,
                                   A5.Indirect (A5.rSP, autosize), 
                                   A5.Imsr (A5.Reg A5.rPC))
           ), A5.AL);
        autosize_opt
     | T.V A.NOP -> raise (Impossible "NOP was removed in step1")

     | T.I (
            ( A5.RFE | A5.Arith _ | A5.ArithF _ | A5.MOVE _
            | A5.SWAP _ | A5.B _ | A5.BL _ | A5.Cmp _ | A5.CmpF _ | A5.Bxx _
            | A5.SWI _
            )
            , _) ->
        autosize_opt
  ) None;

  (* works by side effect, still return first node *)
  cg
(*e: function [[Rewrite5.rewrite]] *)
(*e: Rewrite5.ml *)
