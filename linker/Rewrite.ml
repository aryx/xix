(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec find_first_no_nop_node nopt =
  match nopt with
  | None -> failwith "could not find non NOP node for branch"
  | Some (n : 'a T.node) ->
      (match n.instr with
      | T.Virt A.NOP -> find_first_no_nop_node n.next
      | _ -> Some n
      )
