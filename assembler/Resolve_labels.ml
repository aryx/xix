(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Resolve and remove the LabelDef/LabelUse constructs as well as
 * the branch_operand Relative (turned into Absolute like for LabelUse).
 *
 * As you can see, the Plan 9 assembler does not do much. Most of the work
 * is done in the linker really. 
 *)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let error (s : string) (line : int) =
  (* TODO: use Location_cpp.Error instead! *)
  failwith (spf "%s at line %d" s line)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* ocaml: see how little processing 5a actually does :) *)
let resolve branch_opd_of_instr (prog : 'instr program) : 'instr program =
  let pc : virt_pc ref = ref 0 in
  let h = Hashtbl.create 101 in

  let (ps, locs) = prog in

  (* first pass, process the label definitions, populate h *)
  ps |> List.iter (fun (p, line) ->
    (match p with
    | LabelDef lbl -> 
        (* better to check duplicate here than via lexing tricks *)
        if Hashtbl.mem h lbl
        then error (spf "redeclaration of %s" lbl) line;

        Hashtbl.add h lbl !pc;
        (* no incr pc; share pc if multiple labels at same place *)
    | Instr _ | Pseudo (TEXT _ | WORD _) -> 
        incr pc
    | Pseudo (DATA _ | GLOBL _ ) -> ()
    | Virtual (RET | NOP) -> ()
    )
  );

  (* second pass, process the label uses, resolve some branch operands *)
  pc := 0;
  (* filter but modify also by side effect p *)
  let ps' = ps |> List.filter (fun (p, line) ->
    (match p with
    (* no need to keep the labels in the object file *)
    | LabelDef _ -> false

    | Pseudo (TEXT _ | WORD _) -> 
        incr pc; 
        true
    | Pseudo (DATA _ | GLOBL _) -> 
        (* no pc increment here *)
        true
    | Virtual (RET | NOP) -> 
        incr pc; 
        true
    | Instr instr ->

        (* TODO? move nested function out? *)
        let resolve_branch_operand (opd : branch_operand2 ref) : unit =
          match !opd with
          | SymbolJump _ | IndirectJump _ -> ()
          (* Relative and LabelUse -> Absolute *)
          | Relative i -> 
              opd := Absolute (!pc + i)
          | LabelUse (lbl, i) ->
              (try
                 let pc = Hashtbl.find h lbl in
                 (* less: could keep label info also for debugging purpose? *)
                 opd := Absolute (pc + i)
               with Not_found ->
                 error (spf "undefined label: %s" lbl) line
               )
          | Absolute _ -> 
              raise (Impossible "Absolute can't be made via assembly syntax")
        in
        branch_opd_of_instr instr |> Option.iter resolve_branch_operand;

        incr pc;
        true
    )
  )
  in
  ps', locs
