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
  mutable offsets: (Ast.fullname, int) Hashtbl.t;
  (* reference counting registers used (size = 16), 
   * really a (A.register, int) Hashtbl.t;
   *)
  mutable regs: int array;
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
 | ConstI of integer
 | Register of A.register

 (* indirect *)
 | Name of Ast.fullname
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
let noattr = { A.prof = false; A.dupok = false}

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

let set_instr env pc instr loc =
  if pc >= env.pc
  then failwith (spf "set_instr: pc > env.pc (%d >= %d)" pc env.pc);
  env.code.(pc) <- (instr, loc)


let add_fake_instr env str =
  let spc = env.pc in
  add_instr env (A.LabelDef (str ^ "(fake)")) fake_loc;
  spc
 

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

let symbol fullname = Ast.unwrap fullname

let mov_operand env opd =
  match opd.opd with
  | ConstI i   -> A.Imsr (A.Imm i)
  | Register r -> A.Imsr (A.Reg r)
  | Name fullname ->
    let idinfo = Hashtbl.find env.ids fullname in
    (match idinfo.TC.sto with
    | S.Param -> 
      let offset = Hashtbl.find env.offsets fullname in
      A.Param (Some (symbol fullname), offset)
    | S.Local ->
      let offset = Hashtbl.find env.offsets fullname in
      (* - offset for locals *)
      A.Local (Some (symbol fullname), - offset)
    | S.Static | S.Global | S.Extern ->
      (* less: can have non 0 offset?? *)
      let offset = 0 in
      A.Entity (entity_of_id fullname idinfo, offset)
    )
  | Indirect _ -> raise Todo
  (* todo: OIND, OADDR, ADD *)

(*****************************************************************************)
(* Operand able, instruction selection  *)
(*****************************************************************************)

(* less: should do the OADDR/OIND reduction before *)
let operand_able env e0 =
  let kind_opt = 
    match e0.e with
    | Int (s, inttype) -> Some (ConstI (int_of_string s))
    (* todo: float handling *)
    | Float _ -> None
    | Id fullname -> Some (Name fullname)
    | Unary (op, e) ->
      (match op, e.e with
      | GetRef, _ -> None
      | DeRef, _  -> raise Todo
      | (UnPlus | UnMinus | Tilde), _ -> 
        raise (Impossible "should have been converted")
      | Not, _ -> raise Todo
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

let reguse env (A.R x) =
  env.regs.(x) <- env.regs.(x) + 1

let regfree env (A.R x) = 
  env.regs.(x) <- env.regs.(x) - 1;
  if env.regs.(x) <= 0
  then raise (Error (E.ErrorMisc ("error in regfree", fake_loc)))
  
let regalloc env =
  (* less: lasti trick? *)
  raise Todo

let opd_regalloc env opd tgtopt =
  raise Todo

let opd_regfree env opd =
  raise Todo

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

let gopcode env =
  raise Todo

let rec gmove env opd1 opd2 =
  match opd1.opd with
  (* a load *)
  | Name _ | Indirect _ ->
    let move_size = 
      match opd1.typ with
      | T.I (T.Int, _) -> A.Word
      | _ -> raise Todo
    in
    (* less: opti which does opd_regfree env opd2 (Some opd2)? worth it? *)
    let opd1reg = opd_regalloc env opd1 (Some opd2) in
    gmove_aux env move_size opd1 opd1reg;
    gmove env opd1reg opd2;
    opd_regfree env opd1reg
  | _ ->
    (match opd2.opd with
    (* a store *)
    | Name _ | Indirect _ ->
      let move_size =
        match opd2.typ with
        | T.I (T.Int, _) -> A.Word
        | _ -> raise Todo
      in
      (* less: opti which does opd_regfree env opd2 (Some opd1)?? *)
      let opd2reg = opd_regalloc env opd2 None in
      gmove env opd1 opd2reg;
      gmove_aux env move_size opd2reg opd2;
      opd_regfree env opd2reg

    | _ -> 
      (* the simple case *)
      let move_size = 
        match opd1.typ, opd2.typ with
        | T.I (T.Int, _), T.I (T.Int, _) -> A.Word
        (* todo: lots of opti related to float *)
        | _ -> raise Todo
      in
      gmove_aux env move_size opd1 opd2
    )

(* At this point, either opd1 or opd2 references memory, but not both,
 * so we can do the move in one instruction.
 *)
and gmove_aux env move_size opd1 opd2 =
  add_instr env 
    (A.Instr (A.MOVE (move_size, None, 
                      mov_operand env opd1,
                      mov_operand env opd2), A.AL)) opd1.loc

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo: inrel ? 
 * todo: if complex type node
 *)
let rec expr env e0 dst_opd_opt =

  match operand_able env e0, dst_opd_opt with
  | Some opd1, Some opd2 -> gmove env opd1 opd2
  | Some _, None -> 
     (* less: should have warned about unused opd in check.ml *)
     ()
  | None, _ ->
    (match e0.e with
    | Sequence (e1, e2) -> 
      expr env e1 None;
      expr env e2 dst_opd_opt
    
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
      let dst = { opd = Register rRET; typ = e.e_type; loc = e.e_loc } in
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
    (* todo: if Flag.profile (can be disabled by #pragma) *)
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
    env.size_locals <- 0;
    env.offset_locals <- 0;
    env.offsets <- offsets;
    env.regs <- Array.copy regs_initial;
    stmt env st;

    set_instr env spc 
      (A.Pseudo (A.TEXT (entity_of_id fullname idinfo, attrs, env.size_locals)))
      loc;
    add_instr env (A.Instr (A.RET, A.AL)) loc;

    (* sanity check register allocation *)
    env.regs |> Array.iteri (fun i v ->
      if regs_initial.(i) <> v
      then raise (Error (E.ErrorMisc (spf "reg %d left allocated" i, loc)));
    );

  );

  (* todo: generate code for ids after *)

  (Array.sub env.code 0 (env.pc) |> Array.to_list) @
  List.rev env.data
