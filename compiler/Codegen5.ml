(* Copyright 2016, 2017 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast
module C = Ast
module A = Ast_asm
module A5 = Ast_asm5

module T = Type
module S = Storage
module TC = Typecheck
module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *   - funcalls
 *   - fields, structures
 * todo later:
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

  ids:     (Ast.fullname, TC.idinfo) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;

  (* less: compute offset for each field?
   * fields: (Ast.fullname * string, A.offset) Hashtbl.t
   *)

  arch: Arch.t;

  (* the output *)

  pc: Ast_asm.virt_pc ref;

  (* growing array *)
  code: (A5.line * Ast_asm.loc) array ref;
  (* should contain only DATA or GLOBL *)
  data: (A5.line * Ast_asm.loc) list ref;

  (* reinitialized for each function *)

  (* reference counting the used registers (size = 16), 
   * really a (A.register, int) Hashtbl.t;
   *)
  regs: int array;

  size_locals: int ref;
  offset_locals: int ref;
  (* for parameters and locals *)
  offsets: (Ast.fullname, int) Hashtbl.t;

  (* for goto/labels *)
  labels: (string, Ast_asm.virt_pc) Hashtbl.t;
  (* if the label is defined after the goto, when we process the label,
   * we need to update previous goto instructions.
   *)
  forward_gotos: (string, Ast_asm.virt_pc list) Hashtbl.t;

  (* for loops (and switch) *)
  (* reinitialized for each block scope *)
  break_pc: Ast_asm.virt_pc option;
  continue_pc: Ast_asm.virt_pc option;

}

let rRET = R 0
(* opti: let rARG = A.R 0 *)

(* for 'extern register xx;', used in ARM kernel *)
let rEXT1 = R 10
let rEXT2 = R 9

let regs_initial = 
  let arr = Array.make A5.nb_registers 0 in
  [A5.rLINK; A5.rPC;       (* hardware reseved *)
   A5.rTMP; A5.rSB; A5.rSP; (* linker reserved *)
   rEXT1; rEXT2;         (* compiler reserved *)
  ] |> List.iter (fun (R x) ->
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
 | Register of register

 (* indirect *)
 | Name of Ast.fullname * offset
 | Indirect of register * offset
 (* was not "addressable" in original 5c, but I think it should *)
 | Addr of Ast.fullname



type error = Check.error
let string_of_error err =
  Check.string_of_error err
exception Error of error


(*****************************************************************************)
(* Instructions  *)
(*****************************************************************************)

let fake_instr = A.Virtual A.NOP
let fake_loc = -1
let fake_pc = -1
let noattr = { prof = false; dupok = false}

let add_instr env instr loc = 
  (* grow array if necessary *)
  if !(env.pc) >= Array.length !(env.code)
  then begin
    let increment = 100 in
    let newcode = 
      Array.make (Array.length !(env.code) + increment)  (fake_instr, fake_loc)
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
  add_instr env (A.Instr (A5.B (ref (Absolute fake_pc)), A5.AL)) loc;
  spc
 
let patch_fake_goto env pcgoto pcdest =
  match !(env.code).(pcgoto) with
  (* TODO? what about BL? time to factorize B | BL | Bxx ? *)
  (* ocaml-light: | A5.Instr (A5.B aref, A5.AL), _loc | A5.Instr (A5.Bxx (_, aref), A5.AL), _loc *)
  | A.Instr (A5.B aref, A5.AL), _loc ->
    if !aref = (Absolute fake_pc)
    then aref := Absolute pcdest
    else raise (Impossible "patching already resolved branch")
  | A.Instr (A5.Bxx (_, aref), A5.AL), _loc ->
    if !aref = (Absolute fake_pc)
    then aref := Absolute pcdest
    else raise (Impossible "patching already resolved branch")
  | _ -> raise (Impossible "patching non jump instruction")


(*****************************************************************************)
(* C to Asm helpers  *)
(*****************************************************************************)

let global_of_id fullname idinfo = 
  { name = Ast.unwrap fullname;
    priv = 
      (match idinfo.TC.sto with
      | S.Static -> Some (-1)
      | S.Global | S.Extern -> None
      | _ -> raise (Impossible "global can be only Static/Global/Extern")
      );
    (* less: analyse idinfo.typ *)
    signature = None;
  }

let symbol fullname = Ast.unwrap fullname

let entity_of_id env fullname offset_extra =
  let idinfo = Hashtbl.find env.ids fullname in
  match idinfo.TC.sto with
  | S.Param -> 
    let offset = Hashtbl.find env.offsets fullname + offset_extra in
    Param (Some (symbol fullname), offset)
  | S.Local ->
    let offset = Hashtbl.find env.offsets fullname + offset_extra in
    (* - offset for locals *)
    A.Local (Some (symbol fullname), - offset)
  | S.Static | S.Global | S.Extern ->
    let offset = offset_extra in
    A.Global (global_of_id fullname idinfo, offset)


(* less: opportunity for bitshifted registers? *)
let mov_operand_of_opd env opd =
  match opd.opd with
  | ConstI i   -> A5.Imsr (A5.Imm i)
  | Register r -> A5.Imsr (A5.Reg r)
  | Name (fullname, offset) -> A5.Entity (entity_of_id env fullname offset)
  | Indirect (r, offset) -> A5.Indirect (r, offset)
  | Addr fullname -> A5.Ximm (A.Address (entity_of_id env fullname 0))

let arith_instr_of_op op r1 r2 r3 =
  A5.Arith (
    (match op with
    | Arith op ->
      (match op with 
      | Plus -> A5.ADD | Minus -> A5.SUB
      | And -> A5.AND | Or -> A5.ORR | Xor -> A5.EOR
      (* todo: need type info for A.SLR *)
      | ShiftLeft -> A5.SLL | ShiftRight -> A5.SRA
      (* todo: need type info for A.MULU, etc *)
      | Mul -> A5.MUL | Div -> A5.DIV | Mod -> A5.MOD
      )
    | Logical _ -> raise Todo
    ),
    None, 
    A5.Reg r1, Some r2, r3
  )

(*****************************************************************************)
(* Operand able, instruction selection  *)
(*****************************************************************************)

let operand_able e0 =
  let kind_opt = 
    match e0.e with
    | Int (s, _inttype) -> Some (ConstI (int_of_string s))
    (* todo: float handling *)
    | Float _ -> None
    | Id fullname -> Some (Name (fullname, 0))
    | Unary (op, e) ->
      (match op with
      (* special case to handle *(&arr + <cst>) *)
      | DeRef  ->
        (match e.e with
        (* less: this should be handled in rewrite.ml *(&x) ==> x *)
        | (Unary (GetRef, { e = Id fullname; e_loc=_;e_type=_ })) -> Some (Name (fullname, 0))
        (* less: should normalize constant to left or right in rewrite.ml *)
        | Binary ({ e = Int (s1, _); e_loc=_;e_type=_ }, 
                  Arith Plus, 
                  {e = (Unary (GetRef, { e = Id fullname; e_loc=_;e_type=_ })); e_loc=_;e_type=_ })
          -> Some (Name (fullname, int_of_string s1))
        | Binary ({e = (Unary (GetRef, { e = Id fullname; e_loc=_;e_type=_ })); e_loc=_;e_type=_ }, 
                  Arith Plus, 
                  { e = Int (s1, _); e_loc=_;e_type=_a })
          -> Some (Name (fullname, int_of_string s1))
        | _ -> None
        )

      | GetRef -> 
        (match e.e with
        (* why 5c does not make OADDR (ONAME) an addressable node? *)
        | Id fullname -> Some (Addr fullname)
        | _ -> None
        )
   
      | (UnPlus | UnMinus | Tilde) -> 
        raise (Impossible "should have been converted")
      | Not -> None
      )
    (* less: could be operand_able if we do constant_evaluation later *)
    | Binary (_e1, _op, _e2) -> None
    | Call _ | Assign _ | Postfix _ | Prefix _ | CondExpr _ | Sequence _
      -> None
    
    | Cast _ -> raise Todo
    | RecordAccess _ -> raise Todo
    
    | String _ | ArrayAccess _ | RecordPtAccess _ | SizeOf _ -> 
      raise (Impossible "should have been converted")
    | ArrayInit _ | RecordInit _ | GccConstructor _ -> 
      None
  in
  match kind_opt with
  | None -> None
  | Some opd -> Some { opd = opd; typ = e0.e_type; loc = e0.e_loc }

let fn_complexity = 100 

(* less: could optimize by caching result in node, so no need
 * call again complexity on subtree later
 *)
let rec complexity e =
  if operand_able e <> None
  then 0
  else 
    match e.e with
    | Int _ | Float _ | String _ | Id _ -> 0
    | Call _ -> fn_complexity
    | Assign _ | ArrayAccess _ | Binary _ | Sequence _ ->
      let (e1, e2) =
        match e.e with
        | Assign (_, e1, e2) -> e1, e2
        | ArrayAccess (e1, e2) -> e1, e2
        | Binary (e1, _, e2) -> e1, e2
        | Sequence (e1, e2) -> e1, e2
        | _ -> raise (Impossible "see pattern match above")
      in
      let n1 = complexity e1 in
      let n2 = complexity e2 in
      if n1 = n2
      then 1 + n1
      else max n1 n2

    | CondExpr (e1, e2, e3) -> 
      complexity {e with e = Sequence (e1, { e with e = Sequence (e2, e3) } ) }

    | RecordAccess _ | RecordPtAccess _ | Cast _ | Postfix _ | Prefix _ | Unary _
      -> 
       let e =
         match e.e with
         | RecordAccess (e, _) -> e
         | RecordPtAccess (e, _) -> e
         | Cast (_, e) -> e
         | Postfix (e, _) -> e
         | Prefix (_, e) -> e
         | Unary (_, e) -> e
         | _ -> raise (Impossible "see pattern match above")
      in
      let n = complexity e in
      if n = 0 then 1 else n
    (* should be converted in Int anyway *)
    | SizeOf _ -> 0

    | ArrayInit _ | RecordInit _ | GccConstructor _ -> raise Todo


(*****************************************************************************)
(* Register allocation helpers *)
(*****************************************************************************)

let reguse env (A.R x) =
  env.regs.(x) <- env.regs.(x) + 1

let regfree env (A.R x) = 
  env.regs.(x) <- env.regs.(x) - 1;
  if env.regs.(x) < 0
  then raise (Error (E.Misc ("error in regfree", fake_loc)))

let with_reg env r f =
  (* less: care about exn? meh, if exn then no recovery anyway *)
  reguse env r;
  let res = f() in
  regfree env r;
  res
  
let regalloc env loc =
  (* less: lasti trick? *)
  let rec aux i n =
    (* This happens in extreme case when the expression tree has a huge
     * depth everywhere. In that case, we should allocate a new temporary
     * on the stack but this complexifies the algorithm.
     *)
    if i >= n 
    then raise (Error(E.Misc("out of fixed registers; rewrite your code",loc)));

    if env.regs.(i) = 0
    then begin
      env.regs.(i) <- 1;
      i
    end
    else aux (i+1) n
  in
  aux 0 (Array.length env.regs)


(* We can reuse a previous register if 'tgtopt' is a register.
 * See for example return.c where we can reuse R0 instead of a new R1.
 *)
let opd_regalloc env typ loc tgtopt =
  match typ with
  | T.I _ | T.Pointer _ ->
    let i = 
      match tgtopt with
      | Some { opd = Register (A.R x); typ=_; loc=_ } -> 
        reguse env (A.R x);
        x
      | _ -> regalloc env loc
    in
    { opd = Register (A.R i); typ; loc }
  | _ -> raise Todo

(*
let opd_regalloc_opd env opd tgtopt =
  opd_regalloc env opd.typ opd.loc tgtopt
let opd_regalloc_e env e tgtopt =
  opd_regalloc env e.e_type e.e_loc tgtopt
*)

let opd_regfree env opd =
  match opd.opd with
  | Register r -> regfree env r;
  | _ -> raise (Impossible "opd_regfree on non-register operand")


(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

(* Even though two arguments are operand_able, it does not mean
 * we can move one into the other with one instruction. 
 * In theory, 5a supports general MOVW, but 5l restricts those
 * MOVW to only store and load (not both at the same time).
 * This is why we must decompose below the move in 2 instructions
 * sometimes.
 *)
let rec gmove env opd1 opd2 =
  match opd1.opd with
  (* a load *)
  | Name _ | Indirect _ ->
    let move_size = 
      match opd1.typ with
      | T.I (T.Int, _) | T.Pointer _ -> A5.Word
      | _ -> raise Todo
    in
    (* less: opti which does opd_regfree env opd2 (Some opd2)? worth it? *)
    let opd1reg = opd_regalloc env opd1.typ opd1.loc (Some opd2) in
    gmove_aux env move_size opd1 opd1reg;
    gmove env opd1reg opd2;
    opd_regfree env opd1reg
  | _ ->
    (match opd2.opd with
    (* a store *)
    | Name _ | Indirect _ ->
      let move_size =
        match opd2.typ with
        | T.I (T.Int, _) | T.Pointer _ -> A5.Word
        | _ -> raise Todo
      in
      (* less: opti which does opd_regfree env opd2 (Some opd1)?? *)
      let opd2reg = opd_regalloc env opd2.typ opd2.loc None in
      gmove env opd1 opd2reg;
      gmove_aux env move_size opd2reg opd2;
      opd_regfree env opd2reg

    | _ -> 
      (* the simple cases *)
      let move_size = 
        match opd1.typ, opd2.typ with
        | T.I (T.Int, _), T.I (T.Int, _) -> A5.Word
        | T.Pointer _, T.Pointer _ -> A5.Word
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
    (A.Instr (A5.MOVE (move_size, None, 
                      mov_operand_of_opd env opd1,
                      mov_operand_of_opd env opd2), A5.AL)) opd1.loc

let gmove_opt env opd1 opd2opt = 
  match opd2opt with
  | Some opd2 -> gmove env opd1 opd2
  | None ->
    (* less: should have warned about unused opd in check.ml *)
    ()


(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo: inrel ? 
 * todo: if complex type node
 * less: dst_opd always a register? maybe, but still need to carry
 *  also its type and loc, so maybe easier to always wrap it in an operand?
 *)
let rec expr env e0 dst_opd_opt =

  match operand_able e0 with
  | Some opd1 -> gmove_opt env opd1 dst_opd_opt
  | None ->
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
        let n1 = complexity e1 in
        let n2 = complexity e2 in
        let opdres, opdother = 
          if n1 >= n2
          then begin
            let opd1reg = opd_regalloc env e1.e_type e1.e_loc dst_opd_opt in
            expr env e1 (Some opd1reg);
            let opd2reg = opd_regalloc env e2.e_type e2.e_loc None in
            expr env e2 (Some opd2reg);
            (match opd1reg.opd, opd2reg.opd with
            | Register r1, Register r2 ->
              (* again reverse order SUB r2 r1 ... means r1 - r2 *)
              add_instr env (A.Instr (arith_instr_of_op op r2 r1 r1, A5.AL)) 
                e0.e_loc;
            | _ -> raise (Impossible "both operands comes from opd_regalloc")
            );
            opd1reg, opd2reg
          end
          else begin
            let opd2reg = opd_regalloc env e2.e_type e2.e_loc dst_opd_opt in
            expr env e2 (Some opd2reg);
            let opd1reg = opd_regalloc env e1.e_type e1.e_loc None in
            expr env e1 (Some opd1reg);
            (match opd1reg.opd, opd2reg.opd with
            | Register r1, Register r2 ->
              (* This time we store result in r2! important and subtle.
               * This avoids some extra MOVW; see plus_chain.c
               *)
              add_instr env (A.Instr (arith_instr_of_op op r2 r1 r2, A5.AL)) 
                e0.e_loc;
            | _ -> raise (Impossible "both operands comes from opd_regalloc")
            );
            opd2reg, opd1reg
          end
        in
        (* This is why it is better for opdres to be the register
         * allocated from dst_opd_opt so the MOVW below can become a NOP
         * and be removed.
         *)
        gmove_opt env opdres dst_opd_opt;

        opd_regfree env opdres;
        opd_regfree env opdother;
            
      | Logical _ ->
        raise Todo
      )
    
    | Assign (op, e1, e2) ->
      (match op with
      | SimpleAssign ->
        (match operand_able e1, operand_able e2, dst_opd_opt with
        (* ex: x = 1; *)
        | Some opd1, Some opd2, None -> 
          (* note that e1=e2 -->  MOVW opd2,opd1, (right->left -> left->right)*)
          gmove env opd2 opd1

        (* ex: return x = 1;, x = y = z, ... *)
        | Some _opd1, Some _opd2, Some _dst ->
          raise Todo


        (* ex: y = &x;, y = x + y, ... *)
        | Some opd1, None, None ->
          let opd2reg = opd_regalloc env e2.e_type e2.e_loc None in
          expr env e2 (Some opd2reg);
          gmove env opd2reg opd1;
          opd_regfree env opd2reg;

        (* ex: return x = x+y;, x = y = z, ... *)
        | Some opd1, None, Some dst ->
          let opd2reg = opd_regalloc env e2.e_type e2.e_loc None in
          expr env e2 (Some opd2reg);
          gmove env opd2reg opd1;
          gmove env opd2reg dst; (* only diff with case above *)
          opd_regfree env opd2reg;


        (* ex: *x = 1; *)
        | None, _, _ ->
          raise Todo
        )
      | OpAssign _op ->
        raise Todo
      )
    | Unary (op, e) ->
      (match op with
      | GetRef -> 
        (match e.e  with
        | Id _fullname -> 
          raise (Impossible "handled in operand_able()")
        | Unary (DeRef, _) ->
          raise (Impossible "should be simplified in rewrite.ml")
        | _ -> 
          raise (Impossible "not an lvalue?")
        )

      | DeRef ->
        (* less: opti of Deref of Add with constant? *)
        let opd1reg = opd_regalloc env e.e_type e.e_loc dst_opd_opt in
        expr env e (Some opd1reg);
        gmove_opt env
          (match opd1reg.opd with
          | Register r -> { opd = Indirect (r, 0); loc = e.e_loc;
                            typ = e0.e_type }
          | _ -> raise (Impossible "opd_regalloc_e returns always Register")
          ) dst_opd_opt;
        opd_regfree env opd1reg;
        

      | (UnPlus | UnMinus | Tilde) -> 
        raise (Impossible "should have been converted")
      | Not -> raise Todo
      )
    | _ -> 
      Logs.err (fun m -> m "%s" (Dumper_.s_of_any (Expr e0)));
      raise Todo
    )

let expr_cond env e0 =
  (* todo: *)
  with_reg env rRET (fun () ->
    let dst = { opd = Register rRET; typ = e0.e_type; loc = e0.e_loc } in
    expr env e0 (Some dst);
    (* less: actually should be last loc of e0 *)
    let loc = e0.e_loc in
    add_instr env (A.Instr (A5.Cmp (A5.CMP, A5.Imm 0, rRET), A5.AL)) loc;
    let pc = !(env.pc) in
    add_instr env (A.Instr (A5.Bxx (A5.EQ,(ref (A.Absolute fake_pc))),A5.AL)) loc;
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
  | Var { v_name = fullname; v_loc=_;v_storage=_;v_type=_;v_init=_} ->
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
      add_instr env (A.Virtual A.RET) st0.s_loc
    | Some e ->
      (* todo: if type compatible with R0 *)
      with_reg env rRET (fun () ->
        let dst = { opd = Register rRET; typ = e.e_type; loc = e.e_loc } in
        expr env e (Some dst);
        add_instr env (A.Virtual A.RET) st0.s_loc
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
    add_instr env (A.Instr (A5.B (ref (A.Absolute dstpc)), A5.AL)) st0.s_loc;
    
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
    add_instr env (A.Instr (A5.B (ref(A.Absolute goto_for_continue)), A5.AL)) loc;
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
    add_instr env (A.Instr (A5.B (ref(A.Absolute goto_for_continue)), A5.AL)) loc;
    patch_fake_goto env goto_for_break !(env.pc)

  | Break -> 
    (match env.break_pc with
    | Some dst ->
      add_instr env (A.Instr (A5.B (ref(A.Absolute dst)), A5.AL)) st0.s_loc;
    | None -> raise (Impossible "should be detected in check.ml")
    )
  | Continue ->
    (match env.continue_pc with
    | Some dst ->
      add_instr env (A.Instr (A5.B (ref(A.Absolute dst)), A5.AL)) st0.s_loc;
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
    add_instr env (A.Instr (A5.B (ref(A.Absolute goto_for_continue)), A5.AL)) loc;
    patch_fake_goto env goto_for_break !(env.pc)

  | Switch _ 
  | Case _ | Default _ -> 
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

  funcs |> List.iter (fun { f_name=name; f_loc; f_body=st; f_type=typ; f_storage=_ } ->
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
    let xs = List_.zip typparams tparams in
    let offset = ref 0 in
    xs |> List.iter (fun (p, t) ->
      let sizet = env.arch.Arch.width_of_type {Arch.structs = env.structs} t in
      p.p_name |> Option.iter (fun fullname ->
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
      (A.Pseudo (A.TEXT (global_of_id fullname idinfo, attrs, 
                         !(env.size_locals)))) f_loc;
    add_instr env (A.Virtual A.RET) f_loc;

    (* sanity check register allocation *)
    env.regs |> Array.iteri (fun i v ->
      if regs_initial.(i) <> v
      then raise (Error (E.Misc (spf "reg %d left allocated" i, f_loc)));
    );
  );

  (* todo: generate code for ids after, for CGLOBAL *)

  (Array.sub !(env.code) 0 !(env.pc) |> Array.to_list) @
  List.rev !(env.data)
