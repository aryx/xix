(*s: Arch5.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(* Arm 7500 *)

open Common
open Eq.Operators
open Arch_compiler
module C = Ast
module A = Ast_asm
module T = Type

module A5 = Ast_asm5

(*s: function [[Arch5.width_of_type]] *)
let rec width_of_type (env : Arch_compiler.env) (t : Type.t) : int =
  match t with
  | T.Void -> 0
  | T.I (inttype, _sign) ->
    (match inttype with
    | T.Char -> 1
    | T.Short -> 2
    | T.Int -> 4
    | T.Long -> 4
    | T.VLong -> 8
    )
  | T.F T.Float -> 4
  | T.F T.Double -> 8
  | T.Pointer _ -> 4

  | T.Func _ -> raise (Impossible "width of Func")
  | T.Array (iopt, t) ->
    (match iopt with
    | None -> raise (Impossible "width of incomplete array")
    | Some i -> i * width_of_type env t
    )
  (* TODO: if union then width is not a sum but a max! *)
  | T.StructName (_su, fullname) ->
      let (_su, flds) = Hashtbl.find env.structs fullname in
      (* todo: align so extra size *)
      flds 
      |> List.map (fun (_fld, t) -> width_of_type env t)
      |> List.fold_left (+) 0
(*e: function [[Arch5.width_of_type]] *)

(* opti: let rARGopt = Some (A.R 0) *)

(* for 'extern register xx;', used in ARM kernel *)
let rEXT1 = A.R 10
let rEXT2 = A.R 9

let regs_initial = 
  let arr = Array.make A5.nb_registers 0 in
  (* note that rRET is not in the list; it can be used! *)
  [A5.rLINK; A5.rPC;       (* hardware reseved *)
   A5.rTMP; A5.rSB; A5.rSP; (* linker reserved *)
   rEXT1; rEXT2;         (* compiler reserved *)
  ] |> List.iter (fun (A.R x) ->
    arr.(x) <- 1
  );
  arr

(* alt: add binaryOp to Ast_asm.ml so generalize to a few archs 
 * 5c: part of gopcode()
*)
let arith_instr_of_op (op : C.binaryOp) r1 r2 r3 =
  let r2_opt = 
    if r2 =*= r3
    then None
    else Some r2
  in

  A5.Arith (
    (match op with
    | C.Arith op ->
      (match op with 
      | C.Plus -> A5.ADD | C.Minus -> A5.SUB
      | C.And -> A5.AND | C.Or -> A5.ORR | C.Xor -> A5.EOR
      (* todo: need type info for A.SLR *)
      | C.ShiftLeft -> A5.SLL | C.ShiftRight -> A5.SRA
      (* todo: need type info for A.MULU, etc *)
      | C.Mul -> A5.MUL | C.Div -> A5.DIV | C.Mod -> A5.MOD
      )
    | C.Logical _ -> raise Todo
    ),
    None, 
    A5.Reg r1, r2_opt, r3
  ), A5.AL

(* 5c: part of naddr()
 * less: opportunity for bitshifted registers? *)
let mov_operand_of_opd entity_of_id (opd : opd) : A5.mov_operand =
  match opd.opd with
  | ConstI i   -> A5.Imsr (A5.Imm i)
  | Register r -> A5.Imsr (A5.Reg r)
  | Name (fullname, offset) -> A5.Entity (entity_of_id fullname offset)
  | Indirect (r, offset) -> A5.Indirect (r, offset)
  | Addr fullname -> A5.Ximm (A.Address (entity_of_id fullname 0))

(* 5c: part of gins() *)
let move_instr_of_opds entity_of_id (move_size : A.move_size) 
     (opd1: opd) (opd2: opd) : A5.instr_with_cond =
  A5.MOVE (move_size, None, 
                      mov_operand_of_opd entity_of_id opd1,
                      mov_operand_of_opd entity_of_id opd2), A5.AL

(*s: constant [[Arch5.arch]] *)
let arch = { 
    width_of_type;
    regs_initial;
    rSP = A5.rSP;
    rRET = A5.rRET;
    arith_instr_of_op;
    move_instr_of_opds;
  }
(*e: constant [[Arch5.arch]] *)
(*e: Arch5.ml *)
