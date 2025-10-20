(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm
open Ast_asm5
module T = Types
module T5 = Types5

open Types5

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let build_graph (symbols : T.symbol_table) (xs : T5.code array) : T5.code_graph =
  let len = Array.length xs in

  (* stricter: does not make sense to allow empty programs *)
  if len = 0
  then failwith "empty program";

  (* graph initialization *)
  let nodes : T5.node array = xs |> Array.map (fun (instr, loc) ->
    { T. instr = instr; next = None; branch = None; loc = loc; real_pc = -1 }
  )
  in

  (* set the next fields *)
  nodes |> Array.iteri (fun i n ->
    if i+1 < len
    then n.T.next <- Some nodes.(i+1)
  );

  (* set the branch fields *)
  nodes |> Array.iter (fun n ->
    match n.T.instr with
    | T.TEXT _ | T.WORD _ -> ()
    | T.I (inst, _condXXX) ->
        let resolve_branch_operand opd =
          match !opd with
          | IndirectJump _ -> None
          | Relative _ | LabelUse _ ->
              raise (Impossible "Relative and LabelUse resolved by assembler")
          | SymbolJump x ->
              (* resolve branching to symbols *)
              (match (T.lookup_global x symbols).T.section with
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
        let adjust_virt_pc (virt_pc : T.virt_pc) =
          if virt_pc < len
          then n.T.branch <- Some nodes.(virt_pc)
          else failwith (spf "branch out of range %d at %s" virt_pc
                           (T.s_of_loc n.loc))
        in
        (match inst with
        (* ocaml-light: | B opd | BL opd | Bxx (_, opd) ->  *)
        | B opd -> resolve_branch_operand opd |> Option.iter adjust_virt_pc
        | BL opd -> resolve_branch_operand opd |> Option.iter adjust_virt_pc
        | Bxx (_, opd) -> resolve_branch_operand opd |> Option.iter adjust_virt_pc
        | _ -> ()
        )
  );

  nodes.(0)
 
