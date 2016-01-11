(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5
module T = Types
module T5 = Types5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * No need for optab/oplook/ocmp/cmp. Just use pattern matching!
 * (but we might not win or even lose on the bit manip stuff)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let offset_to_R12 x =
  raise Todo

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

type operand_class =
  | CReg
  | CBranch
  | CConst of const_class
  | CSymbol of symbol_class
  | CIndirectReg of indirect_class
  | CAuto of auto_class
  | CAddr of addr_class
  and const_class =
    | Rot
    | NRot
    | LConst
  and symbol_class =
    | Xxx
  and indirect_class = 
    { small_offset: bool;
      rototable: bool;
    }
  and auto_class = bool (* is_small *)
  and addr_class = bool (* TODO *)
(* TODO: embed the value with the constructor? *)

let immrot x =
  raise Todo

let immaddr x =
  raise Todo

let const_class_of_integer x =
  match () with
  | _ when immrot x <> None -> Rot
  | _ when immrot (lnot x) <> None -> NRot
  | _ -> LConst

let indirect_class_of_offset x =
  { small_offset = immaddr x <> None;
    rototable = immrot x <> None;
  }

let symbol_class_of_entity symbols2 (ent, offset) =
  let v = raise Todo in
  let v = offset_to_R12 (v + offset) in
  if immaddr v <> None
  then raise Todo

(* need autosize! *)
let auto_class_of_auto autosize (is_param, offset) =
  if is_param
  then 
    let v = autosize + offset in
    if immaddr v <> None
    then raise Todo
    else raise Todo
  else
    let _v = autosize + 4 + offset in
    raise Todo

let aclass_of_imm_or_ximm x =
  raise Todo


(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

(* give opportunity to sanity check if conflicts *)
type composed_word = (int * int) list

let gcond cond =
  match cond with
  | EQ            -> (0x0, 28)
  | NE            -> (0x1, 28)
  | GE (Unsigned) -> (0x2, 28)
  | LT (Unsigned) -> (0x3, 28)
  | MI            -> (0x4, 28)
  | PL            -> (0x5, 28)
  | VS            -> (0x6, 28)
  | VC            -> (0x7, 28)
  | GT (Unsigned) -> (0x8, 28)
  | LE (Unsigned) -> (0x9, 28) 
  | GE (Signed)   -> (0xa, 28) 
  | LT (Signed)   -> (0xb, 28)
  | GT (Signed)   -> (0xc, 28)
  | LE (Signed)   -> (0xd, 28)
  | AL            -> (0xe, 28)
  | NV            -> (0xf, 28)


let gop_arith op opt =
  (match op with
  | AND -> [0x0, 21]
  | EOR -> [0x1, 21]
  | SUB -> [0x2, 21]
  | RSB -> [0x3, 21]
  | ADD -> [0x4, 21]
  | ADC -> [0x5, 21]
  | SBC -> [0x6, 21]
  | RSC -> [0x7, 21]
  (* TST 0x8, TEQ 0x9, CMP 0xa, CMN 0xb *)
  | ORR -> [0xc, 21]
  (* MOV 0xd *)
  | BIC -> [0xe, 21]
  | MVN -> [0xf, 21]

  | MUL 
  | DIV 
  | MOD -> raise (Impossible "should match those cases before")

  | SLL 
  | SRL 
  | SRA -> raise (Impossible "should transform those cases")

  ) @ 
  (match opt with
  | None -> []
  | Some Set_condition -> [(1, 20)]
  )

let gop_cmp op =
  match op with
  | TST -> [(0x8, 21); (1, 20)]
  | TEQ -> [(0x9, 21); (1, 20)]
  | CMP -> [(0xa, 21); (1, 20)]
  | CMN -> [(0xb, 21); (1, 20)]

(*
let (<<) = (lsl)
let (>>) = (lsr)
(* less: could make sure does not interfere! *)
let (|@) = (lor)
*)

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)

type action = {
  size: int;
  pool: int option;
  binary: unit -> composed_word list;
}

(* conventions (matches the one used (inconsistently) in 5l):
 * - rf = register from (called Rm in refcard)
 * - rt = register to   (called Rd in refcard)
 * - r = register middle (called Rn in refcard)
 *)
let rules symbols2 x =
  match x.T5.node with
  (* was kept just for better error reporting localisation *)
  | T5.TEXT (_, _, _) -> 
      { size = 0; pool = None; binary = (fun () -> []) }

  (* todo: actually write more rules with WORD instead of doing in aclass *)
  | T5.WORD _ -> 
      { size = 4; pool = None; binary = (fun () -> [raise Todo]) }

  | T5.I (instr, cond) ->
    (match instr with
    | Arith ((AND|ORR|EOR|ADD|SUB|BIC|ADC|SBC|RSB|RSC|MVN) as op, opt, 
             from, middle, (R rt)) ->
        let r =
          if op = MVN
          then 0
          else match middle with None -> rt | Some (R x) -> x 
        in
        let from_part =
          match from with
          | Reg (R rf) -> [(rf, 0)]
          | _ -> raise Todo
        in
        { size = 4; pool = None; binary = (fun () ->
          [[gcond cond]@ gop_arith op opt @ [(r, 16); (rt, 12)] @ from_part]
          )}

    | Cmp (op, from, (R r)) ->
        let from_part = 
          match from with
          | Reg (R rf) -> [(rf, 0)]
          | _ -> raise Todo
        in
        { size = 4; pool = None; binary = (fun () ->
            [[gcond cond] @ gop_cmp op @ [(r, 16); (0, 12)] @ from_part]
          )}
        
    | MOV (Word, _, Imsr (Reg (R rf)), Imsr (Reg (R rt))) -> 
        raise Todo

    | _ -> raise Todo
    )

(*****************************************************************************)
(* entry points *)
(*****************************************************************************)

let size_of_instruction symbols2 node =
  let action  = rules symbols2 node in
  action.size



let gen_one cg instr =
  raise Todo


(* TODO: double check pc is like one computed by layout_text 
   otherwise failwith  "phase error ..."
*)
let gen symbols cg =
  raise Todo


