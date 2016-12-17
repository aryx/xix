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
 *   - fields, structures
 *   - firstarg opti
 *   - alignment 
 *     * fields (sualign)
 *     * parameters
 *   - other integer types and cast
 *   - float
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Environment for code generation *)
type env = {

  (* computed by previous typechecking phase *)

  ids:  (Ast.fullname, TC.idinfo) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
  (* less: compute offset for each field?
   * fields: (Ast.fullname * string, A.offset) Hashtbl.t
   *)

  arch: Arch.arch;

  (* the output *)

  pc: A.virt_pc ref;

  (* growing array *)
  code: (A.line * A.loc) array ref;
  (* should contain only DATA or GLOBL *)
  data: (A.line * A.loc) list ref;

  (* reinitialized for each function *)

  (* reference counting registers used (size = 16), 
   * really a (A.register, int) Hashtbl.t;
   *)
  regs: int array;

  size_locals: int ref;
  offset_locals: int ref;

  (* for parameters and locals *)

  offsets: (Ast.fullname, int) Hashtbl.t;

  (* for goto/labels *)

  labels: (string, A.virt_pc) Hashtbl.t;
  (* if the label is defined after the goto, when we process the label,
   * we need to update the previous goto instructions.
   *)
  forward_gotos: (string, A.virt_pc list) Hashtbl.t;

  (* for loops (and switch) *)

  (* reinitialized for each block scope *)
  break_pc: A.virt_pc option;
  continue_pc: A.virt_pc option;

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
 | Name of Ast.fullname * A.offset
 | Indirect of A.register * A.offset

 | Addr of Ast.fullname



type error = Check.error
let string_of_error err =
  Check.string_of_error err
exception Error of error


(*****************************************************************************)
(* Instructions  *)
(*****************************************************************************)

let fake_instr = A.Instr (A.NOP, A.AL)
let fake_loc = -1
let fake_pc = -1
let noattr = { A.prof = false; A.dupok = false}

let add_instr env instr loc = 
  (* grow array if necessary *)
  if !(env.pc) >= Array.length !(env.code)
  then begin
    let newcode = 
      Array.make (Array.length !(env.code) + 100)  (fake_instr, fake_loc)
    in
    Array.blit !(env.code) (Array.length !(env.code)) newcode 0 0;
    env.code := newcode
  end;

  !(env.code).(!(env.pc)) <- (instr, loc);
  incr env.pc;
  ()

let set_instr env pc instr loc =
  if pc >= !(env.pc)
  then failwith (spf "set_instr: pc > env.pc (%d >= %d)" pc !(env.pc));
  !(env.code).(pc) <- (instr, loc)


let add_fake_instr env str =
  let spc = !(env.pc) in
  add_instr env (A.LabelDef (str ^ "(fake)")) fake_loc;
  spc

let add_fake_goto env loc =
  let spc = !(env.pc) in
  add_instr env (A.Instr (A.B (ref (A.Absolute fake_pc)), A.AL)) loc;
  spc
 
let patch_fake_goto env pcgoto pcdest =
  match !(env.code).(pcgoto) with
  | A.Instr (A.B aref, A.AL), _loc 
  | A.Instr (A.Bxx (_, aref), A.AL), _loc
    ->
    if !aref = (A.Absolute fake_pc)
    then aref := A.Absolute pcdest
    else raise (Impossible "patching already resolved branch")
  | _ -> raise (Impossible "patching non jump instruction")


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
  (* less: opportunity for bitshifted registers? *)
  | Name (fullname, offset_extra) ->
    let idinfo = Hashtbl.find env.ids fullname in
    (match idinfo.TC.sto with
    | S.Param -> 
      let offset = Hashtbl.find env.offsets fullname + offset_extra in
      A.Param (Some (symbol fullname), offset)
    | S.Local ->
      let offset = Hashtbl.find env.offsets fullname + offset_extra in
      (* - offset for locals *)
      A.Local (Some (symbol fullname), - offset)
    | S.Static | S.Global | S.Extern ->
      (* less: can have non 0 offset?? *)
      let offset = offset_extra in
      A.Entity (entity_of_id fullname idinfo, offset)
    )
  | Indirect _ -> raise Todo
  | Addr fullname -> 
    let idinfo = Hashtbl.find env.ids fullname in
    A.Ximm (A.Address (entity_of_id fullname idinfo))


(*****************************************************************************)
(* Operand able, instruction selection  *)
(*****************************************************************************)

(* less: rename to mov_operand_able? because $name+x can be an operand
 *  but not a mov operand?
 *)
let operand_able env e0 =
  let kind_opt = 
    match e0.e with
    | Int (s, _inttype) -> Some (ConstI (int_of_string s))
    (* todo: float handling *)
    | Float _ -> None
    | Id fullname -> Some (Name (fullname, 0))
    | Unary (op, e) ->
      (match op with
      | DeRef  ->
        (match e.e with
        (* less: this should be handled in rewrite.ml *(& x) ==> x *)
        | (Unary (GetRef, { e = Id fullname })) -> Some (Name (fullname, 0))
        (* less: should normalize constant to left or right in rewrite.ml *)
        | Binary ({ e = Int (s1, _) }, 
                  Arith Plus, 
                  {e = (Unary (GetRef, { e = Id fullname })) })
        | Binary ({e = (Unary (GetRef, { e = Id fullname })) }, 
                  Arith Plus, 
                  { e = Int (s1, _) })
          -> Some (Name (fullname, int_of_string s1))
        | _ -> None
        )

      (* todo: Why does not consider OADDR (ONAME) as operand_able, even though
       *  there is a Addr case in operand_able type? because
       *  OADDR requires special treatment?
       *)
      | GetRef -> 
        (match e.e with
        (* todo: why 5c does not make OADDR (ONAME) an addable >= INDEXED? *)
        | Id fullname -> Some (Addr fullname)
        | _ -> None
        )
   
      | (UnPlus | UnMinus | Tilde) -> 
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

let reguse env (A.R x) =
  env.regs.(x) <- env.regs.(x) + 1

let regfree env (A.R x) = 
  env.regs.(x) <- env.regs.(x) - 1;
  if env.regs.(x) < 0
  then raise (Error (E.ErrorMisc ("error in regfree", fake_loc)))

let with_reg env r f =
  (* less: care about exn? meh, if exn then no recovery anyway *)
  reguse env r;
  let res = f() in
  regfree env r;
  res
  
let regalloc env =
  (* less: lasti trick? *)
  let rec aux i n =
    (* todo: impossible? can reach this point? *)
    if i >= n 
    then raise (Impossible "out of fixed registers");

    if env.regs.(i) = 0
    then begin
      env.regs.(i) <- 1;
      i
    end
    else aux (i+1) n
  in
  aux 0 (Array.length env.regs)


(* todo: can reuse previous register if thtopt is a register 
 * see for example return.c for miss opportunity to reuse R0 instead of R1
 *)
let opd_regalloc env opd _tgtoptTODO =
  match opd.typ with
  | T.I _ | T.Pointer _ ->
    let i = regalloc env in
    { opd with opd = Register (A.R i) }
  | _ -> raise Todo

(* less: maybe right to generalize expr and operand_able in 5c? *)
let opd_regalloc_e env e _tgtoptTODO =
  match e.e_type with
  | T.I _ | T.Pointer _ ->
    let i = regalloc env in
    { opd = Register (A.R i); typ = e.e_type; loc = e.e_loc }
  | _ -> raise Todo



let opd_regfree env opd =
  match opd.opd with
  | Register (A.R i) ->
    env.regs.(i) <- env.regs.(i) - 1;
    if env.regs.(i) < 0
    then raise (Error (E.ErrorMisc ("error in regfree", opd.loc)));
  | _ -> raise (Impossible "opd_regfree on non-register operand")


(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

(* Even though two arguments are operand_able, it does not mean
 * we can move one into the other with one instruction. Indeed,
 * 5a supports in theory general MOVW, but 5l restricts those
 * MOVW to only store and load (not both at the same time).
 * This is why below we must decompose the move in 2 instructions
 * sometimes.
 *)
let rec gmove env opd1 opd2 =
  match opd1.opd with
  (* a load *)
  | Name _ | Indirect _ ->
    let move_size = 
      match opd1.typ with
      | T.I (T.Int, _) | T.Pointer _ -> A.Word
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
        | T.I (T.Int, _) | T.Pointer _ -> A.Word
        | _ -> raise Todo
      in
      (* less: opti which does opd_regfree env opd2 (Some opd1)?? *)
      let opd2reg = opd_regalloc env opd2 None in
      gmove env opd1 opd2reg;
      gmove_aux env move_size opd2reg opd2;
      opd_regfree env opd2reg

    | _ -> 
      (* the simple cases *)
      let move_size = 
        match opd1.typ, opd2.typ with
        | T.I (T.Int, _), T.I (T.Int, _) -> A.Word
        | T.Pointer _, T.Pointer _ -> A.Word
        (* todo: lots of opti related to float *)
        | _ -> raise Todo
      in
      gmove_aux env move_size opd1 opd2
    )

(* At this point, either opd1 or opd2 references memory (but not both),
 * so we can do the move in one instruction.
 *)
and gmove_aux env move_size opd1 opd2 =
  (* less: should happen only for register? *)
  if opd1.opd = opd2.opd
  then ()
  else 
  add_instr env 
    (A.Instr (A.MOVE (move_size, None, 
                      mov_operand env opd1,
                      mov_operand env opd2), A.AL)) opd1.loc

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo: inrel ? 
 * todo: if complex type node
 * less: dst_opd always a register? maybe, but still need to carry
 *  also its type and loc, so maybe easier to always wrap it in an operand.
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
        (match operand_able env e1, operand_able env e2, dst_opd_opt with
        (* ex: x = 1; *)
        | Some opd1, Some opd2, None -> 
          (* note that e1=e2 -->  MOVW opd2,opd1, (right->left -> left->right)*)
          gmove env opd2 opd1

        (* ex: return x = 1;, x = y = z, ... *)
        | Some opd1, Some opd2, Some dst ->
          raise Todo


        (* ex: y = &x;, y = x + y, ... *)
        | Some opd1, None, None ->
          let opd2reg = opd_regalloc_e env e2 None in
          expr env e2 (Some opd2reg);
          gmove env opd2reg opd1;
          opd_regfree env opd2reg;

        (* ex: return x = x+y;, x = y = z, ... *)
        | Some opd1, None, Some dst ->
          let opd2reg = opd_regalloc_e env e2 None in
          expr env e2 (Some opd2reg);
          gmove env opd2reg opd1;
          gmove env opd2reg dst; (* only diff with case above *)
          opd_regfree env opd2reg;


        (* ex: *x = 1; *)
        | _ -> raise Todo
        )
      | OpAssign op ->
        raise Todo
      )
    | Unary (op, e) ->
      (match op with
      | GetRef -> 
        raise (Impossible "handled in operand_able()")
      | DeRef ->
        raise Todo

      | (UnPlus | UnMinus | Tilde) -> 
        raise (Impossible "should have been converted")
      | Not -> raise Todo
      )
    | _ -> 
      pr2 (Dumper.s_of_any (Expr e0));
      raise Todo
    )

let expr_cond env e0 =
  (* todo: *)
  with_reg env rRET (fun () ->
    let dst = { opd = Register rRET; typ = e0.e_type; loc = e0.e_loc } in
    expr env e0 (Some dst);
    (* less: actually should be last loc of e0 *)
    let loc = e0.e_loc in
    add_instr env (A.Instr (A.Cmp (A.CMP, A.Imm 0, rRET), A.AL)) loc;
    let pc = !(env.pc) in
    add_instr env (A.Instr (A.Bxx (A.EQ,(ref (A.Absolute fake_pc))),A.AL)) loc;
    pc
  )

let expropt env eopt =
  match eopt with
  | None -> ()
  | Some e -> expr env e None

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
      env.offset_locals := !(env.offset_locals) + sizet;
      env.size_locals := !(env.size_locals) + sizet;
      Hashtbl.add env.offsets fullname !(env.offset_locals);
  | Return eopt ->
    (match eopt with
    | None ->
      add_instr env (A.Instr (A.RET, A.AL)) st0.s_loc
    | Some e ->
      (* todo: if type compatible with R0 *)
      with_reg env rRET (fun () ->
        let dst = { opd = Register rRET; typ = e.e_type; loc = e.e_loc } in
        expr env e (Some dst);
        add_instr env (A.Instr (A.RET, A.AL)) st0.s_loc
      )
    )

  | Label (name, st) ->
    let here = !(env.pc) in
    Hashtbl.add env.labels name here;
    if Hashtbl.mem env.forward_gotos name
    then begin
      let xs = Hashtbl.find env.forward_gotos name in
      xs |> List.iter (fun xpc -> patch_fake_goto env xpc here);
      (* not really necessary because can not define the same label twice *)
      Hashtbl.remove env.forward_gotos name
    end;
    (* todo? generate dummy Goto +1? *)
    stmt env st
  | Goto name ->
    let here = !(env.pc) in
    let dstpc = 
      if Hashtbl.mem env.labels name
      then Hashtbl.find env.labels name
      else begin
        Hashtbl.replace env.forward_gotos name
          (here:: (if Hashtbl.mem env.forward_gotos name
                  then Hashtbl.find env.forward_gotos name
                  else []));
        fake_pc
      end
    in
    add_instr env (A.Instr (A.B (ref (A.Absolute dstpc)), A.AL)) st0.s_loc;
    
  | If (e, st1, st2) ->
    let goto_else_or_end = ref (expr_cond env e) in
    if st1.s <> Block []
    then stmt env st1;
    if st2.s <> Block []
    then begin
      let goto_end = add_fake_goto env st2.s_loc in
      patch_fake_goto env !goto_else_or_end !(env.pc);
      stmt env st2;
      goto_else_or_end := goto_end;
    end;
    patch_fake_goto env !goto_else_or_end !(env.pc)

  | While (e, st) ->
    let goto_entry        = add_fake_goto env e.e_loc in
    let goto_for_continue = add_fake_goto env e.e_loc in
    let goto_for_break    = add_fake_goto env e.e_loc in
    patch_fake_goto env goto_for_continue !(env.pc);
    patch_fake_goto env goto_entry !(env.pc);
    
    let goto_else = expr_cond env e in
    patch_fake_goto env goto_else goto_for_break;

    let env = { env with 
      break_pc = Some goto_for_break; 
      continue_pc = Some goto_for_continue;
    }
    in
    stmt env st;

    (* less: should be last loc of st? *)
    let loc = e.e_loc in
    add_instr env (A.Instr (A.B (ref(A.Absolute goto_for_continue)), A.AL)) loc;
    patch_fake_goto env goto_for_break !(env.pc)

  | DoWhile (st, e) ->

    let goto_entry        = add_fake_goto env e.e_loc in
    let goto_for_continue = add_fake_goto env e.e_loc in
    let goto_for_break    = add_fake_goto env e.e_loc in
    patch_fake_goto env goto_for_continue !(env.pc);
    (* for a while: patch_fake_goto env goto_entry env.pc; *)
    
    let goto_else = expr_cond env e in
    patch_fake_goto env goto_else goto_for_break;
    (* for a dowhile! *)
    patch_fake_goto env goto_entry !(env.pc);

    let env = { env with 
      break_pc = Some goto_for_break; 
      continue_pc = Some goto_for_continue;
    }
    in
    stmt env st;

    (* less: should be last loc of st? *)
    let loc = e.e_loc in
    add_instr env (A.Instr (A.B (ref(A.Absolute goto_for_continue)), A.AL)) loc;
    patch_fake_goto env goto_for_break !(env.pc)

  | Break -> 
    (match env.break_pc with
    | Some dst ->
      add_instr env (A.Instr (A.B (ref(A.Absolute dst)), A.AL)) st0.s_loc;
    | None -> raise (Impossible "should be detected in check.ml")
    )
  | Continue ->
    (match env.continue_pc with
    | Some dst ->
      add_instr env (A.Instr (A.B (ref(A.Absolute dst)), A.AL)) st0.s_loc;
    | None -> raise (Impossible "should be detected in check.ml")
    )

  | For (e1either, e2opt, e3opt, st) ->
    (match e1either with
    | Left e1opt -> expropt env e1opt
    (* todo: scope, should reset autoffset once processed loop *)
    | Right decls -> 
      decls |> List.iter (fun var -> stmt env { s = Var var; s_loc=var.v_loc});
    );
    let goto_entry        = add_fake_goto env st0.s_loc in
    let goto_for_continue = add_fake_goto env st0.s_loc in
    let goto_for_break    = add_fake_goto env st0.s_loc in

    patch_fake_goto env goto_for_continue !(env.pc);
    expropt env e3opt;
    patch_fake_goto env goto_entry !(env.pc);
    (match e2opt with
    | None -> ()
    | Some e2 -> 
      let goto_else = expr_cond env e2 in
      patch_fake_goto env goto_else goto_for_break;
    );
    
    let env = { env with 
      break_pc = Some goto_for_break; 
      continue_pc = Some goto_for_continue;
    }
    in
    stmt env st;

    let loc = st0.s_loc in
    add_instr env (A.Instr (A.B (ref(A.Absolute goto_for_continue)), A.AL)) loc;
    patch_fake_goto env goto_for_break !(env.pc)

  | Switch _ | Case _ | Default _ -> 
    raise Todo


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let codegen (ids, structs, funcs) =

  let env = {
    ids = ids;
    structs = structs;
    arch = Arch5.arch;

    pc = ref 0;
    code = ref [||];
    data = ref [];

    size_locals = ref 0;
    offset_locals = ref 0;
    offsets       = Hashtbl.create 0;
    labels        = Hashtbl.create 0;
    forward_gotos = Hashtbl.create 0;

    break_pc = None;
    continue_pc = None;

    regs          = Array.make 0 16;
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
    let env = { env with
      size_locals = ref 0;
      offset_locals = ref 0;
      offsets       = offsets;
      labels        = Hashtbl.create 11;
      forward_gotos = Hashtbl.create 11;
      regs          = Array.copy regs_initial;
    }
    in
    stmt env st;

    set_instr env spc 
      (A.Pseudo (A.TEXT (entity_of_id fullname idinfo, attrs, 
                         !(env.size_locals)))) loc;
    add_instr env (A.Instr (A.RET, A.AL)) loc;

    (* sanity check register allocation *)
    env.regs |> Array.iteri (fun i v ->
      if regs_initial.(i) <> v
      then raise (Error (E.ErrorMisc (spf "reg %d left allocated" i, loc)));
    );
  );

  (* todo: generate code for ids after, for CGLOBAL *)

  (Array.sub !(env.code) 0 !(env.pc) |> Array.to_list) @
  List.rev !(env.data)
