(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm
open Ast_asm5

let error s line =
  (* TODO: use Location_cpp.Error instead! *)
  failwith (spf "%s at line %d" s line)

(* ocaml: see how little processing 5a actually does :) *)
let resolve (ps : program) : program =
  let pc : virt_pc ref = ref 0 in
  let h = Hashtbl.create 101 in

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
    | Pseudo (DATA _ | GLOBL _) -> ()
    )
  );

  (* second pass, process the label uses, resolve some branch operands *)
  pc := 0;
  (* filter but modify also by side effect p *)
  ps |> List.filter (fun (p, line) ->
    (match p with
    (* no need to keep the labels in the object file *)
    | LabelDef _ -> false

    | Pseudo (TEXT _ | WORD _) -> 
        incr pc; 
        true
    | Pseudo (DATA _ | GLOBL _) -> 
        (* no pc increment here *)
        true
    | Instr (inst, _condTODO) ->

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

        (match inst with
        (* less: could issue warning if cond <> AL when B or Bxx,
         * or normalize?
         *)
        (* ocaml-light: | B opd | BL opd | Bxx (_, opd) -> *)
        | B opd -> resolve_branch_operand opd
        | BL opd -> resolve_branch_operand opd
        | Bxx (_, opd) -> resolve_branch_operand opd
        | _ -> ()
        );
        incr pc;
        true
    )
  )
