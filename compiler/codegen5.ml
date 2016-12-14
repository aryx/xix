(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module C = Ast
module A = Ast_asm5

module T = Type
module S = Storage
module E = Check
module TC = Typecheck

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *   - compute offset for local/params
 *   - firstarg opti
 *   - structure alignment (sualign)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Environment for code generation *)
type env = {
  ids:  (Ast.fullname, TC.idinfo) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;

  arch: Arch.arch;

  mutable pc: A.virt_pc;
  (* growing array *)
  mutable code: (A.line * A.loc) array;
  (* should contain only DATA or GLOBL *)
  mutable data: (A.line * A.loc) list;

  (* reinitialized for each function *)
  mutable size_locals: int;
  mutable offset_locals: int;
  (* for parameters and locals *)
  offsets: (Ast.fullname, int) Hashtbl.t;
}


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake_instr = 
  A.Instr (A.NOP, A.AL)
let fake_loc =
  -1

let noattr = { A.dupok = false; A.prof = false }

let add_instr env instr loc = 
  (* grow array if necessary *)
  if env.pc >= Array.length env.code
  then begin
    let newcode = 
      Array.make (Array.length env.code + 100)  (fake_instr, fake_loc)
    in
    Array.blit env.code (Array.length env.code) newcode 0 0;
    env.code <- newcode
  end;
  env.code.(env.pc) <- (instr, loc);
  env.pc <- env.pc + 1;
  ()

let add_fake_instr env str =
  let spc = env.pc in
  add_instr env (A.LabelDef (str ^ "(fake)")) fake_loc;
  spc

let set_instr env pc instr loc =
  if pc >= env.pc
  then failwith (spf "set_instr: pc > env.pc (%d >= %d)" pc env.pc);
  env.code.(pc) <- (instr, loc)
  

let patch_instr env pcgoto pcdest =
  raise Todo

let entity_of_id fullname idinfo = 
  { A.name = Ast.unwrap fullname;
    A.priv = 
      (match idinfo.TC.sto with
      | S.Static -> Some (-1)
      | S.Global | S.Extern -> None
      | _ -> raise (Impossible "entity can be only Static/Global/Extern")
      );
    (* less: analyse idinfo.typ *)
    A.signature = None;
  }


(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
let rec stmt env st0 =
  if true
  then ()
  else ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* gclean:
 * - generate all AGLOBL
 * - use ast_asm_common.ml?
 *)

let codegen (ids, structs, funcs) =

  let env = {
    ids = ids;
    structs = structs;
    arch = Arch5.arch;

    pc = 0;
    code = [||];
    data = [];

    size_locals = -1;
    offset_locals = -1;
    offsets = Hashtbl.create 0;
  } in

  funcs |> List.iter (fun { f_name=name; f_loc=loc; f_body=st } ->
    let fullname = (name, 0) in
    let idinfo = Hashtbl.find env.ids fullname in
    (* less: can set fields from C? *)
    let attrs = noattr in

    let spc = add_fake_instr env "TEXT" in
    let offsets = Hashtbl.create 11 in
    (* todo: add offsets for paramters
       todo: align offset_locals with return type
    *)
    let newenv = { env with 
      size_locals = 0;
      offset_locals = 0;
      offsets = offsets;
    } in
    stmt newenv st;

    set_instr env spc 
      (A.Pseudo (A.TEXT (entity_of_id fullname idinfo, attrs, 
                         newenv.size_locals)))
      loc;
    add_instr env (A.Instr (A.RET, A.AL)) loc;

  );

  (* todo: generate code for ids after *)

  (Array.sub env.code 0 (env.pc - 1) |> Array.to_list) @
  List.rev env.data


  
