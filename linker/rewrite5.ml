(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

open Ast_asm5
module T = Types
module T5 = Types5


let rec find_first_no_nop_node nopt =
  match nopt with
  | None -> failwith "could not find non NOP node for branch"
  | Some n ->
      (match n.T5.instr with
      | T5.I (NOP, _) -> find_first_no_nop_node n.T5.next
      | _ -> Some n
      )

(* less: rewrite when profiling flag -p *)
let rewrite cg =
  
  (* a set *)
  let is_leaf = Hashtbl.create 101 in

  (* step1: mark is leaf and delete NOPs *)
  cg |> T5.iter_with_env (fun (curtext, prev_no_nop) n ->
    match n.T5.instr with
    | T5.TEXT (ent, _attrs, _size) ->
        Hashtbl.add is_leaf ent true;
        (Some ent, Some n)
    | T5.WORD _ -> (curtext, Some n)
    | T5.I (instr, _condXXX) ->
        let env = 
          match instr with
          | BL _ -> 
              curtext |> Option.iter (fun p -> Hashtbl.remove is_leaf p);
              (curtext, Some n)
          (* remove the NOP *)
          | NOP ->
              prev_no_nop |> Option.iter (fun prev ->
                prev.T5.next <- n.T5.next;
              );
              (curtext, prev_no_nop)
          | _ -> (curtext, Some n)
        in
        n.T5.branch |> Option.iter (fun n2 ->
          match n2.T5.instr with
          | T5.I (NOP, _) -> n.T5.branch <- find_first_no_nop_node n2.T5.next 
          | _ -> ()
        );
        env
  ) (None, None);
  
  (* step2: transform *)
  cg |> T5.iter_with_env (fun autosize_opt n ->
    match n.T5.instr with
    | T5.TEXT (global, attrs, size) ->
        if size mod 4 <> 0
        then failwith (spf "size of locals should be a multiple of 4 for %s"
                         (T5.s_of_global global));
        if size < 0 
        then failwith "TODO: handle size local -4";
        
        let autosize_opt = 
          if size == 0 && Hashtbl.mem is_leaf global
          then None
          else Some (size + 4)
        in
        autosize_opt |> Option.iter (fun autosize ->
          (* for layout text we need to set the final autosize *)
          n.T5.instr <- T5.TEXT (global, attrs, autosize);
          (* MOVW.W R14, -autosize(SP) *)
          let n1 = { T5.
            instr = T5.I (MOVE (Word, Some WriteAddressBase, 
                              Imsr (Reg rLINK), 
                              Indirect (rSP, -autosize)), AL);
            next = n.T5.next;
            branch = None;
            loc = n.T5.loc;
            real_pc = -1;
          }
          in
          n.T5.next <- Some n1;
        );
        autosize_opt

    | T5.WORD _ -> autosize_opt
    | T5.I (RET, cond) ->
        n.T5.instr <- T5.I
          ((match autosize_opt with
           (* B (R14) *)
           | None -> B (ref (IndirectJump (rLINK)))
           (* MOVW.P autosize(SP), PC *)
           | Some autosize -> MOVE (Word, Some PostOffsetWrite,
                                   Indirect (rSP, autosize), 
                                   Imsr (Reg rPC))
           ), cond);
        autosize_opt
    | _ -> autosize_opt        
  ) None;

  (* works by side effect, still return first node *)
  cg

