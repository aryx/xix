(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asmv

module T = Types
module Tv = Typesv
open Types
open Typesv
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mips code generation.
 *
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error node s =
  failwith (spf "%s at %s on %s" s 
              (T.s_of_loc node.n_loc)
              (Tv.show_instr node.instr)
  )
let int_of_bits (n : node) (x : Bits.int32) : int =
  try
    Bits.int_of_bits32 x
  with Failure s -> error n s

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

(* TODO: also 0x7fff, -0x8000, and 0 special cases *)
let constant_kind i =
  if i <= 0xffff
  then Some i
  else None

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

let op (x : int) (y : int) : Bits.t =
  [(x, 3); (y, 0)]

let sp (x : int) (y : int) : Bits.t =
  [(x, 29); (y, 26)]

let opirr (code : arith_opcode) : Bits.t =
  match code with
  | ADD (W, U) -> sp 1 1
  | OR -> sp 1 5
  | _ -> failwith "TODO:opirr"
  
let op_irr (op : arith_opcode) (i : int) (R r2 : reg) (R r3 : reg) : Bits.t =
  opirr op @ [(i land 0xffff, 0); (r2, 21); (r3, 16)]

let op_rrr (op : Bits.t) (R r1 : reg) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(r1, 16); (r2, 21); (r3, 11)]

(*****************************************************************************)
(* More complex code generation helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions (matches the one used (inconsistently) in vl?):
 * - rf? = register from (called ?? in refcard)
 * - rt? = register to   (called ?? in refcard)
 * - r?  = register middle (called ?? in refcard)
 *)

let rules (env : Codegen.env) (init_data : addr option) (node : 'a T.node) =
  match node.instr with

  (* --------------------------------------------------------------------- *)
  (* Virtual *)
  (* --------------------------------------------------------------------- *)
   | T.V (A.RET | A.NOP) -> 
      raise (Impossible "rewrite should have transformed RET/NOP")

  (* --------------------------------------------------------------------- *)
  (* Pseudo *)
  (* --------------------------------------------------------------------- *)
  (* TEXT instructions were kept just for better error reporting localisation 
   * case 0: /* pseudo ops */
   *)
  | T.TEXT (_, _, _) -> 
      { size = 0; pool = None; binary = (fun () -> []) }

  | T.WORD x ->
      { size = 4; pool = None; binary = (fun () -> 
        match x with
        | Float _ -> raise Todo
        | Int i -> [ [(i land 0xffffffff, 0)] ]
        | String _s -> 
            (* stricter? what does 5l do with that? confusing I think *)
            error node "string not allowed with WORD; use DATA"
        | Address (Global (global, _offsetTODO)) -> 
            let v = Hashtbl.find env.syms (T.symbol_of_global global) in
            (match v with
             | T.SText2 real_pc -> [ [(real_pc, 0)] ]
             | T.SData2 (offset, _kind) -> 
                 (match init_data with
                 | None -> raise (Impossible "init_data should be set by now")
                 | Some init_data -> [ [(init_data + offset, 0)] ]
                 )
            )
        | Address (Param _ | Local _) -> raise Todo
      )}


  | T.I instr ->
    (match instr with
    (* --------------------------------------------------------------------- *)
    (* Arithmetics *)
    (* --------------------------------------------------------------------- *)

    (* Constant to register move (move but no memory involved) 
     * case 3:		/* mov $soreg, r ==> or/add $i,o,r */
    *)
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) ->
       (match constant_kind i with
       | Some i -> 
           { size = 4; pool = None; binary = (fun () ->
               let r = R 0 in
               (* TODO: can also be let op = OR if exactly ANDCON *)
               let op = ADD (W, U) in
               [ op_irr op i r rt ]
            ) }
       | None -> failwith "TODO: LCON"
       )

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)
    (* case 18:	/* jmp [r1],0(r2) */ *)
    | JMP { contents = (IndirectJump r2) } ->
        let r = R 0 in
        let op_jmp = op 1 0 in
        { size = 4; pool = None; binary = (fun () -> [ op_rrr op_jmp (R 0) r2 r ]) }

    (* --------------------------------------------------------------------- *)
    (* Memory *)
    (* --------------------------------------------------------------------- *)

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)
    (* case 5:		/* syscall */ *)
    | SYSCALL ->
       { size = 4; pool = None; binary = (fun () -> [op 1 4]) }
    | BREAK ->
       { size = 4; pool = None; binary = (fun () -> [op 1 5]) }

    (* --------------------------------------------------------------------- *)
    (* Other *)
    (* --------------------------------------------------------------------- *)
    |(Arith (_, _, _, R _)|NOR (_, _, _)|ArithMul (_, R _, _, R _)|ArithF _
     |Move1 (_, _, _)| Move2 _
     |RFE _|JAL _|JALReg (R _, _)|JMP _
     |BEQ (_, _, _)|BNE (_, _, _)|Bxx (_, _, _)
     |TLB _
     ) -> 
       failwith (spf "Codegenv: TODO: instr not handled: %s"
                (Tv.show_instr node.instr))

    (*    | _ -> error node "illegal combination"*)
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* TODO: factorize with Codegen5.ml *)
let size_of_instruction  (env : Codegen.env) (node : Tv.node) : int (* a multiple of 4 *) * Codegen.pool option =
  let action  = rules env None node in
  action.size, action.pool

(* TODO: factorize with Codegen5.ml *)
let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : Tv.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in

  let pc = ref config.init_text in

  cg |> T.iter (fun n ->

    let {size; binary; pool = _ }  = 
        rules Codegen.{ syms = symbols2; autosize = !autosize }
        config.init_data n 
    in
    let instrs = binary () in

    if n.real_pc <> !pc
    then raise (Impossible "Phase error, layout inconsistent with codegen");
    if List.length instrs * 4 <> size
    then raise (Impossible (spf "size of rule does not match #instrs at %s"
                              (T.s_of_loc n.n_loc)));

    let xs : Bits.int32 list = instrs |> List.map Assoc.sort_by_val_highfirst in
    
    if !Flags.debug_gen 
    then begin 
      Logs.app (fun m -> m "%s" (Tv.show_instr n.instr));
      Logs.app (fun m -> m "-->");
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.app (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
      Logs.app (fun m -> m ".");
    end;

    let xs = xs |> List.map (fun x -> int_of_bits n x) in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    (* after the resolve phase the size of a TEXT is the final autosize *)
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten




