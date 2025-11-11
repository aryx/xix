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

(* LATER? x - BIG optimisation *)
let offset_to_R30 x =
  x

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

let op (x : int) (y : int) : Bits.t =
  [(x, 3); (y, 0)]

let sp (x : int) (y : int) : Bits.t =
  [(x, 29); (y, 26)]

let opirr_arith_opcode (code : arith_opcode) : Bits.t =
  match code with
  | ADD (W, S) -> sp 1 0
  | ADD (W, U) -> sp 1 1
  | ADD (V, S) -> sp 3 0
  | ADD (V, U) -> sp 3 1

  | SGT S -> sp 1 2
  | SGT U -> sp 1 3
  | AND -> sp 1 4
  | OR -> sp 1 5
  | XOR -> sp 1 6

  | SLL W -> op 0 0
  | SRL W -> op 0 2
  | SRA W -> op 0 3

  | SLL V -> op 7 0
  | SRL V -> op 7 2
  | SRA V -> op 7 3
  | _ -> failwith "TODO:opirr"

let opirr_move2 (code : move2_size) : Bits.t =
  match code with
  | W__ -> sp 5 3
  | V__ -> sp 7 7
  | F__ -> sp 7 1
  | D__ -> failwith "TODO: opirr_move2 D__ = ?"

let oprrr_arith_opcode (code : arith_opcode) : Bits.t =
  match code with
  | ADD (W, S) -> op 4 0
  | ADD (W, U) -> op 4 1
  | ADD (V, S) -> op 5 4
  | ADD (V, U) -> op 5 5

  | SGT S -> op 5 2
  | SGT U -> op 5 3

  | AND -> op 4 4
  | OR -> op 4 5
  | XOR -> op 4 6

  | SUB (W, S) -> op 4 2
  | SUB (W, U) -> op 4 3

  | SLL W -> op 0 4
  | SRL W -> op 0 6
  | SRA W -> op 0 7

  | _ -> failwith "TODO:oprrr"

let _oprrr_mul_opcode (code : mul_opcode) : Bits.t =
  match code with
  | REM S | DIV (W, S) -> op 3 2
  | REM U | DIV (W, U) -> op 3 3
  | MUL (W, S) -> op 3 0
  | MUL (W, U) -> op 3 1
  | DIV (V, S) -> op 3 6
  | DIV (V, U) -> op 3 7

  | _ -> failwith "TODO:oprrr_mul"
  
let op_irr (op : Bits.t) (i : int) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(i land 0xffff, 0); (r2, 21); (r3, 16)]

let op_rrr (op : Bits.t) (R r1 : reg) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(r1, 16); (r2, 21); (r3, 11)]

(*****************************************************************************)
(* More complex code generation helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions (matches the one used (inconsistently) in vl?):
 * - rf = register from (p->from.reg in vl)
 * - rt = register to (p->to.reg in vl)
 * - r_opt  = register middle (optional, p->reg in vl)
 *)

let rules (env : Codegen.env) (init_data : addr option) (node : 'a T.node) =
  match node.instr with

  (* --------------------------------------------------------------------- *)
  (* Virtual *)
  (* --------------------------------------------------------------------- *)
   | T.Virt (A.RET | A.NOP) -> 
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

    (* case 4:		/* add $scon,[r1],r2 */ *)
    | Arith (ADD (W, S) as op, Imm i, r_opt, rt) ->
        (* TODO: C_ADD0CON vs C_ANDCON generate different opcodes *)
        { size = 4; pool = None; binary = (fun () ->
            let v = i in
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode op) v r rt ]
         ) }

    (* case 1:		/* mov[v] r1,r2 ==> OR r1,r0,r2 */ where r1 = RO
     * which was C_ZCON case in vl span.c which was then accepted for C_REG
     * in span.c cmp() and so was matching the entry in optab.c:
     * { AMOVW,	C_REG,	C_NONE,	C_REG,		 1, 4, 0 },
     *)
    | Move2 (W__, (Right (Int 0)), Gen (GReg rt)) ->
       { size = 4; pool = None; binary = (fun () ->
          [ op_rrr (oprrr_arith_opcode OR) rZERO rZERO rt ]
        ) }

    (* Constant to register move (move but no memory involved) 
     * case 3:		/* mov $soreg, r ==> or/add $i,o,r */
    *)
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) ->
       (match constant_kind i with
       | Some i -> 
           { size = 4; pool = None; binary = (fun () ->
               let r = rZERO in
               (* TODO: can also be let op = OR if exactly ANDCON *)
               let op = ADD (W, U) in
               [ op_irr (opirr_arith_opcode op) i r rt ]
            ) }
       | None -> failwith "TODO: LCON"
       )

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)
    (* case 18:	/* jmp [r1],0(r2) */ *)
    | JMP { contents = (IndirectJump r2) } ->
        let r = rZERO in
        let op_jmp = op 1 0 in
        { size = 4; pool = None; binary = (fun () -> 
           [ op_rrr op_jmp rZERO r2 r ]
         ) }

    (* --------------------------------------------------------------------- *)
    (* Memory *)
    (* --------------------------------------------------------------------- *)

    (* Address *)
    | Move2 (W__, Right ximm, Gen (GReg rt)) ->
        (match ximm with
        | Int _ | Float _ -> 
           failwith "TODO: ?? because of refactor of imm_or_ximm"
        | String _ -> 
            (* stricter? what does vl do with that? confusing I think *)
            error node "string not allowed in MOVW; use DATA"
        | Address (Global (global, _offsetTODO)) ->
            let from_part_when_small_offset_to_R30 =
              try 
                let v = Hashtbl.find env.syms (T.symbol_of_global global) in
                match v with
                | T.SData2 (offset, _kind) ->
                    let final_offset = offset_to_R30 offset in
                    (* super important condition! for bootstrapping
                     * setR30 in MOVW $setR30(SB), R30 and not
                     * transform it in ADD offset_set_R30, R30, R30.
                     *)
                    if final_offset = 0 
                    then None
                    else failwith "TODO: final_offset <> 0"
                | T.SText2 _real_pc -> None
              (* layout_text has not been fully done yet so we may have
               * the address of a procedure we don't know yet
               *)
              with Not_found -> None
            in
            (match from_part_when_small_offset_to_R30 with
            | Some _ ->
                 failwith "TODO: from_part_when_small_offset_to_R30 is a Some"
            | None ->
              (* case 19:	/* mov $lcon,r ==> lu+or */ *)
              { size = 8; pool=None; binary=(fun () ->
              (* similar to WORD case *)
              let v = Hashtbl.find env.syms (T.symbol_of_global global) in
              let lcon =
                match v with
                | T.SText2 real_pc -> real_pc
                | T.SData2 (offset, _kind) -> 
                  (match init_data with
                  | None -> raise (Impossible "init_data should be set by now")
                  | Some init_data -> init_data + offset
                  )
                in
                [
                   op_irr (sp 1 7) (lcon lsr 16) rZERO rt;
                   op_irr (opirr_arith_opcode OR) lcon rt rt;
                ]
              )}
            )
        | Address (Local _ | Param _) -> raise Todo
        )

    (* Load *)

    (* Store *)

    (* case 7:		/* mov r, soreg ==> sw o(r) */ *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rt, offset))) ->
        (* TODO: need look for offset if SOREG or LOREG *)
        { size = 4; pool = None; binary = (fun () ->
          let r = rt in
          (* TODO: regoff *)
          let v = offset in
          [ op_irr (opirr_move2 W__) v r rf ]
         ) }

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
      Logs.app (fun m -> m " %.8x: %s (%s)"
                 !pc 
                  (xs |> List.map (fun x -> spf "%.8x" (int_of_bits n x))
                      |> String.concat " ")
                  (Tv.show_instr n.instr));
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.debug (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
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




