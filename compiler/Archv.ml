(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either
open Arch_compiler
module C = Ast
module A = Ast_asm
module T = Type

module Av = Ast_asmv

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

(* opti: let rARGopt = Some (A.R 0) *)

(* for 'extern register xx;', used in ARM kernel *)
(*
let rEXT1 = A.R 25
let rEXT2 = A.R 9
*)

let regs_initial = 
  let arr = Array.make Av.nb_registers 0 in
  (* note that rRET is not in the list; it can be used! *)
  [Av.rLINK; Av.rZERO; (*Av.rPC;*)       (* hardware reseved *)
   Av.rTMP; Av.rSB; Av.rSP; (* linker reserved *)
   (*rEXT1; rEXT2;*)         (* compiler reserved *)
  ] |> List.iter (fun (A.R x) ->
    arr.(x) <- 1
  );
  arr

(* alt: add binaryOp to Ast_asm.ml so generalize to a few archs *)
let arith_instr_of_op (op : C.binaryOp) r1 r2 r3 =
  (* TODO: *)
  let size = Av.W in
  let sign = A.S in
  match op with
  | C.Arith op ->
      (match op with 
      | C.Plus -> Av.Arith (Av.ADD (size, sign), Av.Reg r1, Some r2, r3)
      | C.Minus -> Av.Arith (Av.SUB (size, sign), Av.Reg r1, Some r2, r3)
      | C.And -> Av.Arith (Av.AND, Av.Reg r1, Some r2, r3)
      | C.Or -> Av.Arith (Av.OR, Av.Reg r1, Some r2, r3)
      | C.Xor -> Av.Arith (Av.XOR, Av.Reg r1, Some r2, r3)
      (* todo: need type info for A.SLR *)
      | C.ShiftLeft -> Av.Arith (Av.SLL size, Av.Reg r1, Some r2, r3)
      | C.ShiftRight -> Av.Arith (Av.SRA size, Av.Reg r1, Some r2, r3)
      (* todo: need type info for A.MULU, etc *)
      | C.Mul -> Av.ArithMul (Av.MUL (size, sign), r1, Some r2, r3)
      | C.Div -> Av.ArithMul (Av.DIV (size, sign), r1, Some r2, r3)
      | C.Mod -> raise Todo
      )
  | C.Logical _ -> raise Todo

(* vc: part of naddr() ?
 * less: opportunity for bitshifted registers? *)
let mov_operand_of_opd entity_of_id (opd : opd) : (Av.gen, A.ximm) Either.t =
  match opd.opd with
  | ConstI i   -> Right (A.Int i)
  | Register r -> Left (Av.GReg r)
  | Name (fullname, offset) -> Left (Av.Entity (entity_of_id fullname offset))
  | Indirect (r, offset) -> Left (Av.Indirect (r, offset))
  | Addr fullname -> Right (A.Address (entity_of_id fullname 0))

let move_instr_of_opds entity_of_id (move_size : A.move_size) 
     (opd1: opd) (opd2: opd) : Av.instr =
  match move_size with
  | A.Word ->
     Av.Move2 (Av.W__,  
                      (match mov_operand_of_opd entity_of_id opd1 with
                      | Left gen -> Left (Av.Gen gen)
                      | Right ximm -> Right ximm
                      ),
                      (match mov_operand_of_opd entity_of_id opd2 with
                      | Left gen -> Av.Gen gen
                      | Right _ximm -> raise (Impossible "move store in int")
                      ))
  | A.HalfWord _ | A.Byte _ -> raise Todo

let arch = { 
    width_of_type;
    regs_initial;
    rSP = Av.rSP;
    rRET = Av.rRET;
    arith_instr_of_op;
    move_instr_of_opds;
  }
