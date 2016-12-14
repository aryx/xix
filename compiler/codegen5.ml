(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module C = Ast
module A = Ast_asm5

module T = Type
module S = Storage
module TC = Typecheck
module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo later:
 *   - field, structures
 *   - firstarg opti
 *   - alignment 
 *     * structure (sualign)
 *     * parameters
 *   - float
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
  (* reference counting registers used (size = 16), 
   * really a (A.register, int) Hashtbl.t;
   *)
  regs: int array;
}

let rRET = A.R 0
let rARG = A.R 0

let rEXT1 = A.R 10
let rEXT2 = A.R 9

let regs_initial = 
  let arr = Array.create A.nb_registers 0 in
  [A.rLINK; A.rPC;       (* hardware reseved *)
   A.rTMP; A.rSB; A.rSP; (* linker reserved *)
   rEXT1; rEXT2;         (* compiler reserved *)
  ] |> List.iter (fun (A.R x) ->
    arr.(x) <- 1
  );
  arr
   
  

type integer = int

(* some form of instruction selection *)
type operand_able = 
 { opd: operand_able_kind;
   typ: Type.t;
   loc: Ast.loc;
 }
and operand_able_kind =
 | ConstI of integer (* less: type? *)
 | Register of A.register

 (* indirect *)
 | Name of Ast.fullname (* less: could put idinfo here *)
 | Indirect of A.register * A.offset
(*
 | Addr
 | Ind

 | Add
*)


type error = Check.error
let string_of_error err =
  Check.string_of_error err
exception Error of error


(*****************************************************************************)
(* Instructions  *)
(*****************************************************************************)

let fake_instr = A.Instr (A.NOP, A.AL)
let fake_loc = -1
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

(*****************************************************************************)
(* C to Asm helpers  *)
(*****************************************************************************)

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
(* Operand able, instruction selection  *)
(*****************************************************************************)

let operand_able env e0 =
  let kind_opt = 
    match e0.e with
    (* less: put also type? *)
    | Int (s, inttype) -> 
      Some (ConstI (int_of_string s))
    (* todo: float handling *)
    | Float _ -> None
    | Id fullname -> Some (Name fullname)
    | Unary (op, e) ->
      (match op with
      | GetRef -> raise Todo
      | DeRef -> raise Todo
      | UnPlus | UnMinus | Tilde -> 
        raise (Impossible "should have been converted")
      | Not -> raise Todo
      )
    | Binary (e1, op, e2) ->
      (match op with
      | Arith Plus -> raise Todo
      | _ -> None
      )
    
    | Call _ | Assign _ | Postfix _ | Prefix _ | CondExpr _ | Sequence _
      -> None
    
    | Cast _ -> raise Todo
    | RecordAccess _ -> raise Todo
    
    | ArrayInit _ | RecordInit _ | GccConstructor _ -> raise Todo
    | String _ | ArrayAccess _ | RecordPtAccess _ | SizeOf _
      -> raise (Impossible "should have been converted")
  in
  match kind_opt with
  | None -> None
  | Some opd -> Some { opd = opd; typ = e0.e_type; loc = e0.e_loc }


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

let gmove env opd1 opd2 =
  raise Todo

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo: inrel ? 
 * todo: if complex type node
 *)
let rec expr env e0 optdst =

  match operand_able env e0, optdst with
  | Some opd, Some reg -> gmove env opd (Register reg)
  | Some _, None -> 
     (* less: should have warned about unused opd in check.ml *)
     ()
  | None, _ ->
    (match e0.e with
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
    )

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
    regs = Array.make 0 16;
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
      regs = Array.copy regs_initial;
    } in
    stmt newenv st;

    set_instr env spc 
      (A.Pseudo (A.TEXT (entity_of_id fullname idinfo, attrs, 
                         newenv.size_locals)))
      loc;
    add_instr env (A.Instr (A.RET, A.AL)) loc;

    newenv.regs |> Array.iteri (fun i v ->
      if regs_initial.(i) <> v
      then raise (Error (E.ErrorMisc (spf "reg %d left allocated" i, loc)));
    );

  );

  (* todo: generate code for ids after *)

  (Array.sub env.code 0 (env.pc - 1) |> Array.to_list) @
  List.rev env.data
