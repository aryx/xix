(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm
open Ast_asm5
module T = Types
module T5 = Types5

(* for field access for ocaml-light *)
open Types
open Types5

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec find_first_no_nop_node nopt =
  match nopt with
  | None -> failwith "could not find non NOP node for branch"
  | Some n ->
      (match n.T.instr with
      | T.I (NOP, _) -> find_first_no_nop_node n.T.next
      | _ -> Some n
      )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* less: rewrite when profiling flag -p *)
let rewrite (cg : T5.code_graph) : T5.code_graph =
  
  (* a set *)
  let is_leaf = Hashtbl.create 101 in

  (* step1: mark is leaf and delete NOPs *)
  cg |> T.iter_with_env (fun (curtext, prev_no_nop) n ->
    match n.T.instr with
    | T.TEXT (ent, _attrs, _size) ->
        Hashtbl.add is_leaf ent true;
        (Some ent, Some n)
    | T.WORD _ -> (curtext, Some n)
    | T.I (instr, _condXXX) ->
        let env = 
          match instr with
          | BL _ -> 
              curtext |> Option.iter (fun p -> Hashtbl.remove is_leaf p);
              (curtext, Some n)
          (* remove the NOP *)
          | NOP ->
              prev_no_nop |> Option.iter (fun prev ->
                prev.T.next <- n.T.next;
              );
              (curtext, prev_no_nop)
          | _ -> (curtext, Some n)
        in
        n.branch |> Option.iter (fun n2 ->
          match n2.instr with
          | T.I (NOP, _) -> n.branch <- find_first_no_nop_node n2.next 
          | _ -> ()
        );
        env
  ) (None, None);
  
  (* step2: transform *)
  cg |> T.iter_with_env (fun autosize_opt n ->
    match n.instr with
    | T.TEXT (global, attrs, size) ->
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (T.s_of_global global));
        if size < 0 
        then failwith "TODO: handle size local -4";
        
        let autosize_opt = 
          if size == 0 && Hashtbl.mem is_leaf global
          then None
          else Some (size + 4)
        in
        autosize_opt |> Option.iter (fun autosize ->
          (* for layout text we need to set the final autosize *)
          n.instr <- T.TEXT (global, attrs, autosize);
          (* MOVW.W R14, -autosize(SP) *)
          let n1 = {
            instr = T.I (MOVE (Word, Some WriteAddressBase, 
                              Imsr (Reg rLINK), 
                              Indirect (rSP, -autosize)), AL);
            next = n.next;
            branch = None;
            loc = n.loc;
            real_pc = -1;
          }
          in
          n.next <- Some n1;
        );
        autosize_opt

    | T.WORD _ -> autosize_opt
    | T.I (RET, cond) ->
        n.instr <- T.I
          ((match autosize_opt with
           (* B (R14) *)
           | None -> B (ref (IndirectJump (rLINK)))
           (* MOVW.P autosize(SP), PC *)
           | Some autosize -> MOVE (Word, Some PostOffsetWrite,
                                   Indirect (rSP, autosize), 
                                   Imsr (Reg rPC))
           ), cond);
        autosize_opt
     | T.I (NOP, _) -> raise (Impossible "NOP was removed in step1")
     | T.I ((RFE|Arith (_, _, _, _, _)|MOVE (_, _, _, _)|SWAP (_, _, _, _)|
   B _|BL _|Cmp (_, _, _)|Bxx (_, _)|SWI _), _) ->
        autosize_opt
  ) None;

  (* works by side effect, still return first node *)
  cg

