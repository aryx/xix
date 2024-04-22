(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

open Ast_asm5
module T = Types
module T5 = Types5

let build_graph symbols xs =
  let len = Array.length xs in

  (* stricter: does not make sense to allow empty programs *)
  if len = 0
  then failwith "empty program";

  (* graph initialization *)
  let nodes = xs |> Array.map (fun (instr, loc) ->
    { T5. instr = instr; next = None; branch = None; loc = loc; real_pc = -1 }
  )
  in

  (* set the next fields *)
  nodes |> Array.iteri (fun i n ->
    if i+1 < len
    then n.T5.next <- Some nodes.(i+1)
  );

  (* set the branch fields *)
  nodes |> Array.iter (fun n ->
    match n.T5.instr with
    | T5.TEXT _ | T5.WORD _ -> ()
    | T5.I (inst, cond) ->
        let resolve_branch_operand opd =
          match !opd with
          | IndirectJump _ -> None
          | Relative _ | LabelUse _ ->
              raise (Impossible "Relative and LabelUse resolved by assembler")
          | SymbolJump x ->
              (* resolve branching to symbols *)
              (match (T5.lookup_global x symbols).T.section with
              | T.SText virt_pc -> 
                  opd := Absolute virt_pc; 
                  Some virt_pc
              | T.SXref -> raise (Impossible "SXRef raised by Check.check")
              (* stricter: 5l converts them to SText 0 to avoid reporting
               * multiple times the same error but we fail early instead.
               *)
              | T.SData _ -> failwith "branching to a data symbol"
              )
          | Absolute virt_pc -> Some virt_pc
        in
        (match inst with
        | B opd | BL opd | Bxx (_, opd) -> 
            resolve_branch_operand opd |> Common.if_some (fun virt_pc ->
              if virt_pc < len
              then n.T5.branch <- Some nodes.(virt_pc)
              else failwith (spf "branch out of range %d at %s" virt_pc
                               (T5.s_of_loc n.T5.loc))
            )
        | _ -> ()
        )
  );

  nodes.(0)
 
