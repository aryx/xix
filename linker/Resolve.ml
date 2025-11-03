(*s: Resolve.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Types
module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Resolve.build_graph]] *)
let build_graph branch_opd_of_instr (symbols : T.symbol_table) (xs : 'instr T.code array) : 'instr T.code_graph =
  let len = Array.length xs in

  (* stricter: does not make sense to allow empty programs *)
  if len = 0
  then failwith "empty program";

  (* graph initialization *)
  let nodes : 'intr T.node array = xs |> Array.map (fun (instr, loc) ->
    { T.instr = instr; next = None; branch = None; n_loc = loc; real_pc = -1 }
  )
  in

  (* set the next fields *)
  nodes |> Array.iteri (fun i n ->
    if i+1 < len
    then n.next <- Some nodes.(i+1)
  );

  (* set the branch fields *)
  nodes |> Array.iter (fun n ->
    match n.instr with
    | T.TEXT _ | T.WORD _ -> ()
    | T.V (A.RET | A.NOP) -> ()
    | T.I instr ->
        let resolve_branch_operand opd =
          match !opd with
          | A.IndirectJump _ -> None
          | A.Relative _ | A.LabelUse _ ->
              raise (Impossible "Relative and LabelUse resolved by assembler")
          | A.SymbolJump x ->
              (* resolve branching to symbols *)
              (match (T.lookup_global x symbols).section with
              | T.SText virt_pc -> 
                  opd := A.Absolute virt_pc; 
                  Some virt_pc
              | T.SXref -> raise (Impossible "SXRef raised by Check.check")
              (* stricter: 5l converts them to SText 0 to avoid reporting
               * multiple times the same error but we fail early instead.
               *)
              | T.SData _ -> failwith "branching to a data symbol"
              )
          | A.Absolute virt_pc -> Some virt_pc
        in
        let adjust_virt_pc (virt_pc : T.virt_pc) =
          if virt_pc < len
          then n.branch <- Some nodes.(virt_pc)
          else failwith (spf "branch out of range %d at %s" virt_pc
                           (T.s_of_loc n.n_loc))
        in
        branch_opd_of_instr instr |> Option.iter (fun opd -> 
            resolve_branch_operand opd |> Option.iter adjust_virt_pc)
  );
  nodes.(0)
(*e: function [[Resolve.build_graph]] *)
 
(*e: Resolve.ml *)
