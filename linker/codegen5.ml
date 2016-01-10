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
let (<<) = (lsl)
let (>>) = (lsr)
let (|@) = (lor)

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)

type action = {
  size: int;
  pool: int option;
  binary: unit -> T.word list;
}


let rules symbols2 x =
  match x.T5.node with
  | T5.TEXT (_, _, _) -> 
      { size = 0; pool = None; binary = (fun () -> []) }
  (* todo: actually write more rules with WORD instead of doing in aclass *)
  | T5.WORD _ -> 
      { size = 4; pool = None; binary = (fun () -> [raise Todo]) }

  | T5.I (instr, cond) ->
      (match instr with
      | _ -> raise Todo
      )

(*****************************************************************************)
(* Entry points *)
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


