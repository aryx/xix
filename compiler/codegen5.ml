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

let rRET = A.R 0
let rARG = A.R 0

(* less:
type addrable = 
 | Const 
 | Name
 | Register
 | IndReg
 | Addr
 | Ind
 | Add
*)

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
(* Register allocation helpers *)
(*****************************************************************************)
(*
let reguse env reg
let regfree env reg
let regalloc env e_orig dst_regopt

*)

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

let gopcode env =
  raise Todo

let gmove env =
  raise Todo

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo: inrel ? 
 * todo: if complex type node
 *)
let rec expr env e0 optdst =

  (* todo: if addrable, then gmove *)

  match e0.e with
  | Sequence (e1, e2) -> 
    expr env e1 None;
    expr env e2 optdst

  (* less: lots of possible opti *)
  | Binary (e1, op, e2) ->
    (match op with
    | Arith (Plus | Minus 
            | And | Or | Xor 
            | ShiftLeft | ShiftRight
            | Mul | Div | Mod) ->
      raise Todo
    | Logical _ ->
      raise Todo
    )

  | Assign (op, e1, e2) ->
    (match op with
    | SimpleAssign ->
      raise Todo
    | OpAssign op ->
      raise Todo
    )

  | _ -> 
    pr2 (Dumper.s_of_any (Expr e0));
    raise Todo

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
let rec stmt env st0 =
  match st0.s with
  | ExprSt e -> expr env e None
  | Block xs -> xs |> List.iter (stmt env)
  | Var { v_name = fullname; } ->
      let idinfo = Hashtbl.find env.ids fullname in
      (* todo: generate code for idinfo.ini *)

      (* update env.offsets *)
      let t = idinfo.TC.typ in
      let sizet = env.arch.Arch.width_of_type {Arch.structs = env.structs} t in
      (* todo: align *)
      Hashtbl.add env.offsets fullname env.offset_locals;
      env.offset_locals <- env.offset_locals + sizet;
  | Return eopt ->
    (match eopt with
    | None ->
      add_instr env (A.Instr (A.RET, A.AL)) st0.s_loc
    | Some e ->
      (* todo: if type compatible with R0 *)
      (* todo: reguse, regfree, with_reg *)
      let dst = rRET in
      expr env e (Some dst);
      add_instr env (A.Instr (A.RET, A.AL)) st0.s_loc
    )
    
  | _ -> 
    pr2 (Dumper.s_of_any (Stmt st0));
    raise Todo


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

  funcs |> List.iter (fun { f_name=name; f_loc=loc; f_body=st; f_type=typ } ->
    let fullname = (name, 0) in
    let idinfo = Hashtbl.find env.ids fullname in
    (* less: can set fields from C? *)
    let attrs = noattr in

    let spc = add_fake_instr env "TEXT" in
    
    (* set offsets for parameters *)
    let offsets = Hashtbl.create 11 in
    let (_typret, (typparams, _varargs)) = typ in
    let t = idinfo.TC.typ in
    let tparams = 
      match t with
      | T.Func (_tret, tparams, _varargs) -> tparams
      | _ -> raise (Impossible "not a FUNC")
    in
    assert (List.length tparams = List.length typparams);
    let xs = Common2.zip typparams tparams in
    let offset = ref 0 in
    xs |> List.iter (fun (p, t) ->
      let sizet = env.arch.Arch.width_of_type {Arch.structs = env.structs} t in
      p.p_name |> Common.if_some (fun fullname ->
        Hashtbl.add offsets fullname !offset
      );
      (* todo: align *)
      offset := !offset + sizet;
    );
    
    
    (* todo: align offset_locals with return type *)
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
