(*s: Codegen.ml *)
(* Copyright 2016, 2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Eq.Operators
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
 * TODO:
 *   - fields, structures
 * LATER:
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

(*s: type [[Codegen.env]] *)
(* Environment for code generation *)
type 'i env = {

  (* computed by previous typechecking phase *)
  ids_:     (Ast.fullname, TC.idinfo) Hashtbl.t;
  structs_: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;

  (* less: compute offset for each field?
   * fields: (Ast.fullname * string, A.offset) Hashtbl.t
   *)
  arch: Arch.t;
  a: Arch_compiler.t;

  (* the output *)

  pc: Ast_asm.virt_pc ref;

  (* growing array *)
  code: ('i Ast_asm.line * A.loc) array ref;
  (* should contain only DATA or GLOBL (TODO? restrict to pseudo_instr?) *)
  data: ('i Ast_asm.line * A.loc) list ref;

  (* reinitialized for each function *)

  (* reference counting the used registers (size = 16), 
   * really a (A.register, int) Hashtbl.t;
   *)
  regs: int array;
  (*s: [[Codegen.env]] other function fields *)
  mutable size_locals: int;
  mutable size_maxargs: int; (* 5c: maxargssafe *)
  (*x: [[Codegen.env]] other function fields *)
  (* for parameters and locals *)
  offsets: (Ast.fullname, int) Hashtbl.t;
  (*x: [[Codegen.env]] other function fields *)
  offset_locals: int ref;
  (*x: [[Codegen.env]] other function fields *)
  (* for goto/labels *)
  labels: (string, Ast_asm.virt_pc) Hashtbl.t;

  (* if the label is defined after the goto, when we process the label,
   * we need to update previous goto instructions.
   *)
  forward_gotos: (string, Ast_asm.virt_pc list) Hashtbl.t;
  (*x: [[Codegen.env]] other function fields *)
  (* for loops (and switch) *)
  (* reinitialized for each block scope *)
  break_pc: Ast_asm.virt_pc option;
  continue_pc: Ast_asm.virt_pc option;
  (*e: [[Codegen.env]] other function fields *)
}
(*e: type [[Codegen.env]] *)

(*s: constant [[Codegen.rRET]] *)
(*e: constant [[Codegen.rRET]] *)
(*s: constant [[Codegen.rEXT1]] *)
(*e: constant [[Codegen.rEXT1]] *)
(*s: constant [[Codegen.rEXT2]] *)
(*e: constant [[Codegen.rEXT2]] *)

(*s: constant [[Codegen.regs_initial]] *)
(*e: constant [[Codegen.regs_initial]] *)

(*s: function [[Codegen.env_of_tp]] *)
let env_of_tp (arch: Arch.t) (tp : Typecheck.typed_program) : 'i env =   
  let arch_compiler: Arch_compiler.t =
    match arch with
    | Arch.Arm -> Arch5.arch;
    | _ -> failwith (spf "unsupported arch: %s" (Arch.to_string arch))
        
  in

  {
    ids_ = tp.ids;
    structs_ = tp.structs;
    arch;
    a = arch_compiler;

    pc = ref 0;
    code = ref [||];
    data = ref [];

    (* TODO: move in nested struct so then can reuse default_xxx () *)
    size_locals = 0;
    size_maxargs = 0;
    offset_locals = ref 0;
    offsets       = Hashtbl_.create ();
    labels        = Hashtbl_.create ();
    forward_gotos = Hashtbl_.create ();
    break_pc = None;
    continue_pc = None;
    regs          = [||];
  }
(*e: function [[Codegen.env_of_tp]] *)
  
(*s: type [[Codegen.integer]] *)
(*e: type [[Codegen.integer]] *)

(*s: type [[Codegen.operand_able]] *)
(* operand *)
type opd = 
 { opd: operand_kind;
   typ: Type.t;
   loc: Ast.loc;
 }
(*e: type [[Codegen.operand_able]] *)
(*s: type [[Codegen.operand_able_kind]] *)
and operand_kind =
 | ConstI of Ast_asm.integer
 | Register of Ast_asm.register

 (* indirect *)
 | Name of Ast.fullname * Ast_asm.offset
 | Indirect of Ast_asm.register * Ast_asm.offset

 (* was not "addressable" in original 5c, but I think it should *)
 | Addr of Ast.fullname
(*e: type [[Codegen.operand_able_kind]] *)


(*s: type [[Codegen.error]] *)
type error = Check.error
(*e: type [[Codegen.error]] *)
(*s: function [[Codegen.string_of_error]] *)
let string_of_error err =
  Check.string_of_error err
(*e: function [[Codegen.string_of_error]] *)
(*s: exception [[Codegen.Error]] *)
exception Error of error
(*e: exception [[Codegen.Error]] *)

(*****************************************************************************)
(* Instructions  *)
(*****************************************************************************)
(*s: constant [[Codegen.fake_instr]] *)
let fake_instr = A.Virtual A.NOP
(*e: constant [[Codegen.fake_instr]] *)
(*s: constant [[Codegen.fake_loc]] *)
let fake_loc = -1
(*e: constant [[Codegen.fake_loc]] *)
(*s: constant [[Codegen.fake_pc]] *)
let fake_pc = -1
(*e: constant [[Codegen.fake_pc]] *)

(*s: function [[Codegen.add_instr]] *)
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
(*e: function [[Codegen.add_instr]] *)

(*s: function [[Codegen.set_instr]] *)
let set_instr env pc instr loc =
  if pc >= !(env.pc)
  then failwith (spf "set_instr: pc > env.pc (%d >= %d)" pc !(env.pc));

  !(env.code).(pc) <- (instr, loc)
(*e: function [[Codegen.set_instr]] *)


(*s: function [[Codegen.add_fake_instr]] *)
let add_fake_instr env str =
  let spc = !(env.pc) in
  add_instr env (A.LabelDef (str ^ "(fake)")) fake_loc;
  spc
(*e: function [[Codegen.add_fake_instr]] *)

(*s: function [[Codegen.add_fake_goto]] *)
let add_fake_goto (env : 'i env) loc =
  let spc = !(env.pc) in
  add_instr env (A.Virtual (A.Jmp (ref (Absolute fake_pc)))) loc;
  spc
(*e: function [[Codegen.add_fake_goto]] *)
 
(*s: function [[Codegen.patch_fake_goto]] *)
let patch_fake_goto (env : 'i env) (pcgoto : A.virt_pc) (pcdest : A.virt_pc) =
  match fst !(env.code).(pcgoto) with
  (* TODO? what about BL? time to factorize B | BL | Bxx ? *)
  (* ocaml-light: | Instr (B aref) | A.Instr (A.Bxx aref) -> ... *)
  | A.Virtual (A.Jmp aref) ->
    if !aref =*= (Absolute fake_pc)
    then aref := Absolute pcdest
    else raise (Impossible "patching already resolved branch")

  | A.Instr (A5.Bxx (_, aref), A5.AL) ->
    if !aref =*= (Absolute fake_pc)
    then aref := Absolute pcdest
    else raise (Impossible "patching already resolved branch")
  | _ -> raise (Impossible "patching non jump instruction")
(*e: function [[Codegen.patch_fake_goto]] *)

(*****************************************************************************)
(* C to Asm helpers  *)
(*****************************************************************************)
(*s: function [[Codegen.global_of_id]] *)
let global_of_id env fullname = 
  let idinfo = Hashtbl.find env.ids_ fullname in
  { name = Ast.unwrap fullname;
    priv = 
      (match idinfo.TC.sto with
      | S.Static -> Some (-1)
      | S.Global | S.Extern -> None
      | S.Local | S.Param ->
          raise (Impossible "global can be only Static/Global/Extern")
      );
    (* less: analyse idinfo.typ *)
    signature = None;
  }
(*e: function [[Codegen.global_of_id]] *)

(*s: function [[Codegen.symbol]] *)
let symbol fullname = Ast.unwrap fullname
(*e: function [[Codegen.symbol]] *)

(*s: function [[Codegen.entity_of_id]] *)
let entity_of_id env fullname offset_extra =
  let idinfo = Hashtbl.find env.ids_ fullname in
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
    A.Global (global_of_id env fullname, offset)
(*e: function [[Codegen.entity_of_id]] *)


(*s: function [[Codegen.mov_operand_of_opd]] *)
(* 5c: part of naddr()
 * less: opportunity for bitshifted registers? *)
let mov_operand_of_opd (env : 'i env) (opd : opd) : A5.mov_operand =
  match opd.opd with
  | ConstI i   -> A5.Imsr (A5.Imm i)
  | Register r -> A5.Imsr (A5.Reg r)
  | Name (fullname, offset) -> A5.Entity (entity_of_id env fullname offset)
  | Indirect (r, offset) -> A5.Indirect (r, offset)
  | Addr fullname -> A5.Ximm (A.Address (entity_of_id env fullname 0))
(*e: function [[Codegen.mov_operand_of_opd]] *)

(* 5c: part of naddr() called from gopcode *)
let branch_operand_of_opd (env : 'i env) (opd : opd) : A.branch_operand2 =
  match opd.opd with
  | Name (fullname, offset) ->
      assert (offset =|= 0);
      A.SymbolJump (global_of_id env fullname)
  | ConstI _ -> raise (Impossible "calling an int can't typecheck")
  | Register _ -> raise (Impossible "calling a register can't typecheck")
  | Indirect (r, offset) -> 
      assert (offset =|= 0);
      A.IndirectJump r
  | Addr _ -> raise Todo

(*s: function [[Codegen.arith_instr_of_op]] *)
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
(*e: function [[Codegen.arith_instr_of_op]] *)

(* 5c: regaalloc but actually didn't allocate reg so was a bad name *)
let argument_operand (env : 'i env) (arg: argument) (curarg : int) : opd =
  (* add 4 for space for REGLINK in callee *)
  { opd = Indirect (env.a.rSP, curarg + 4 (* TODO: SZ_LONG *)) ;
    typ = arg.e_type;
    loc = arg.e_loc;
  }

(*****************************************************************************)
(* Operand able, instruction selection  *)
(*****************************************************************************)
(*s: function [[Codegen.operand_able]] *)
let operand_able (e0 : expr) : opd option =
  let kind_opt = 
    match e0.e with
    | String _ | ArrayAccess _ | RecordPtAccess _ | SizeOf _ -> 
      raise (Impossible "should have been converted")
    (*s: [[operand_able()]] match [[e0.e]] cases *)
    (* less: could be operand_able if we do constant_evaluation later *)
    | Binary (_e1, _op, _e2) -> None
    | Call _ | Assign _ | Postfix _ | Prefix _ | CondExpr _ | Sequence _
      -> None

    | Cast _ -> raise Todo
    | RecordAccess _ -> raise Todo

    | ArrayInit _ | RecordInit _ | GccConstructor _ -> 
      None
    (*x: [[operand_able()]] match [[e0.e]] cases *)
    | Int (s, _inttype) -> Some (ConstI (int_of_string s))
    (*x: [[operand_able()]] match [[e0.e]] cases *)
    (* todo: float handling *)
    | Float _ -> None
    (*x: [[operand_able()]] match [[e0.e]] cases *)
    | Id fullname -> Some (Name (fullname, 0))
    (*x: [[operand_able()]] match [[e0.e]] cases *)
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
    (*e: [[operand_able()]] match [[e0.e]] cases *)
  in
  match kind_opt with
  | None -> None
  | Some opd -> Some { opd; typ = e0.e_type; loc = e0.e_loc }
(*e: function [[Codegen.operand_able]] *)

(*s: constant [[Codegen.fn_complexity]] *)
let fn_complexity = 100 
(*e: constant [[Codegen.fn_complexity]] *)

(*s: function [[Codegen.complexity]] *)
(* less: could optimize by caching result in node, so no need
 * call again complexity on subtree later
 *)
let rec complexity (e : expr) : int =
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
      if n1 =|= n2
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
      if n =|= 0 then 1 else n
    (* should be converted in Int anyway *)
    | SizeOf _ -> 0

    | ArrayInit _ | RecordInit _ | GccConstructor _ -> raise Todo
(*e: function [[Codegen.complexity]] *)

(*****************************************************************************)
(* Register allocation helpers *)
(*****************************************************************************)
(*s: function [[Codegen.reguse]] *)
let reguse env (A.R x) =
  env.regs.(x) <- env.regs.(x) + 1
(*e: function [[Codegen.reguse]] *)

(*s: function [[Codegen.regfree]] *)
let regfree env (A.R x) = 
  env.regs.(x) <- env.regs.(x) - 1;
  if env.regs.(x) < 0
  then raise (Error (E.Misc ("error in regfree", fake_loc)))
(*e: function [[Codegen.regfree]] *)

(*s: function [[Codegen.with_reg]] *)
let with_reg env (r : A.register) f =
  (* less: care about exn? meh, if exn then no recovery anyway *)
  reguse env r;
  let res = f () in
  regfree env r;
  res
(*e: function [[Codegen.with_reg]] *)
  
(*s: function [[Codegen.regalloc]] *)
let regalloc (env : 'i env) loc : int =
  (* less: lasti trick? *)
  let rec aux (i : int) (n : int) : int =
    (* This happens in extreme case when the expression tree has a huge
     * depth everywhere. In that case, we should allocate a new temporary
     * on the stack but this complexifies the algorithm.
     *)
    if i >= n 
    then raise (Error(E.Misc("out of fixed registers; rewrite your code",loc)));

    (* TODO: lasti opti *)
    if env.regs.(i) =|= 0
    then begin
      env.regs.(i) <- 1;
      i
    end
    else aux (i+1) n
  in
  aux 0 (Array.length env.regs)
(*e: function [[Codegen.regalloc]] *)

(*s: function [[Codegen.opd_regalloc]] *)
(* We can reuse a previous register if 'tgtopt' is a register.
 * See for example return.c where we can reuse R0 instead of a new R1.
 *)
let opd_regalloc (env : 'i env) (typ : Type.t) loc (tgtopt : opd option) : opd =
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
(*e: function [[Codegen.opd_regalloc]] *)

(*s: function [[Codegen.opd_regfree]] *)
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
(*e: function [[Codegen.opd_regfree]] *)

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)
(*s: function [[Codegen.gmove]] *)
(* Even though two arguments are operand_able, it does not mean
 * we can move one into the other with one instruction. 
 * In theory, 5a supports general MOVW, but 5l restricts those
 * MOVW to only store and load (not both at the same time).
 * This is why we must decompose below the move in 2 instructions
 * sometimes.
 *)
let rec gmove (env : 'i env) (opd1 : opd) (opd2 : opd) : unit =
  match opd1.opd with

  (* a load *)
  | Name _ | Indirect _ ->
    let move_size = 
      match opd1.typ with
      | T.I (T.Int, _) | T.Pointer _ -> A.Word
      | _ -> raise Todo
    in
    (* less: opti which does opd_regfree env opd2 (Some opd2)? worth it? *)
    let opd1reg = opd_regalloc env opd1.typ opd1.loc (Some opd2) in
    gmove_aux env move_size opd1 opd1reg;
    gmove env opd1reg opd2;
    opd_regfree env opd1reg

  | ConstI _ | Register _ | Addr _  ->

    (match opd2.opd with

    (* a store *)
    | Name _ | Indirect _ ->
      let move_size =
        match opd2.typ with
        | T.I (T.Int, _) | T.Pointer _ -> A.Word
        | _ -> raise Todo
      in
      (* less: opti which does opd_regfree env opd2 (Some opd1)?? *)
      let opd2reg = opd_regalloc env opd2.typ opd2.loc None in
      gmove env opd1 opd2reg;
      gmove_aux env move_size opd2reg opd2;
      opd_regfree env opd2reg

    | ConstI _ | Register _ | Addr _ -> 

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
 * 5c: called gins()
 *)
and gmove_aux env move_size (opd1 : opd) (opd2 : opd) : unit =
  (* less: should happen only for register? *)
  if opd1.opd =*= opd2.opd
  then ()
  else 
  add_instr env 
    (A.Instr (A5.MOVE (move_size, None, 
                      mov_operand_of_opd env opd1,
                      mov_operand_of_opd env opd2), A5.AL)) opd1.loc
(*e: function [[Codegen.gmove]] *)
(*s: function [[Codegen.gmove_opt]] *)
let gmove_opt (env : 'i env) (opd1 : opd) (opd2opt : opd option) :
    unit = 
  match opd2opt with
  | Some opd2 -> gmove env opd1 opd2
  | None ->
    (* SURE? should we still registerize and all? *)
    (* less: should have warned about unused opd in check.ml *)
    ()
(*e: function [[Codegen.gmove_opt]] *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)
(*s: function [[Codegen.expr]] *)
(* todo: inrel ? 
 * todo: if complex type node
 * 5c: called cgen/cgenrel()
 *)
let rec expr (env : 'i env) (e0 : expr) (dst_opd_opt : opd option) : unit=
  match operand_able e0 with
  | Some opd1 -> gmove_opt env opd1 dst_opd_opt
  | None ->
    (match e0.e with
    | Int _ | Float _ | Id _ ->
        raise (Impossible "handled in operand_able()")
    | String _ | ArrayAccess _ | RecordPtAccess _ | SizeOf _ -> 
        raise (Impossible "should have been converted before")    
    (*s: [[Codegen.expr()]] when not operand able, match [[e0.e]] cases *)
    | Sequence (e1, e2) -> 
      expr env e1 None;
      expr env e2 dst_opd_opt
    (*x: [[Codegen.expr()]] when not operand able, match [[e0.e]] cases *)
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
    (*x: [[Codegen.expr()]] when not operand able, match [[e0.e]] cases *)
    | Assign (op, e1, e2) ->
      (match op with
      | Eq_ ->
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
    (*x: [[Codegen.expr()]] when not operand able, match [[e0.e]] cases *)
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
    | Call (e, es) ->
        if complexity e >= fn_complexity
        then 
          (* (foo(...))(...), so function call in e itself *)
          raise Todo
        else begin
          arguments env es;
          (match operand_able e with
          (* complex call *)
          | None -> raise Todo
          | Some opd ->
             add_instr env (A.Virtual (A.JmpAndLink
                            (ref (branch_operand_of_opd env opd)))) e0.e_loc;
             dst_opd_opt |> Option.iter (fun dst_opd ->
                with_reg env env.a.rRET (fun () ->
                    (* TODO? need Cast? 5c does gopcode(OAS, ...) *)
                    let src_opd = { opd = Register env.a.rRET; typ = e0.e_type;
                                    loc = e0.e_loc;} in
                    gmove env src_opd dst_opd
                );
             );
          );
          ()
        end
    (*e: [[Codegen.expr()]] when not operand able, match [[e0.e]] cases *)
    | RecordAccess _
    | Cast _
    | Postfix _ | Prefix _
    | CondExpr _
    | ArrayInit _ | RecordInit _ | GccConstructor _
      -> 
      Logs.err (fun m -> m "%s" (Dumper_.s_of_any (Expr e0)));
      raise Todo
    )
(*e: function [[Codegen.expr]] *)
and arguments (env : 'i env) (xs : argument list) : unit =
  let curarg = ref 0 in
  xs |> List.iter (fun (arg : argument) ->
      (* TODO: if complex argument type or complex arg *)
      (* TODO: if use REGARG and curarg is 0 *)
      let opd = opd_regalloc env arg.e_type arg.e_loc None in
      expr env arg (Some opd);
      let opd2 = argument_operand env arg !curarg in
      let sizet = 
        env.a.width_of_type {Arch_compiler.structs = env.structs_} arg.e_type
      in
      (* TODO: cursafe, curarg align before and after *)
      curarg := !curarg + sizet;
      (* TODO: 5c does gopcode(OAS, tn1, Z, tn2) *)
      gmove env opd opd2;
      opd_regfree env opd
  );
  (* TODO: 4 -> SZ_LONG *)
  env.size_maxargs <- Int_.maxround env.size_maxargs !curarg 4;
  ()

(*s: function [[Codegen.expr_cond]] *)
(* 5c: bcomplex? () *)
let expr_cond (env : 'i env) (e0 : expr) : virt_pc =
  (* todo: *)
  with_reg env env.a.rRET (fun () ->
    let dst = { opd = Register env.a.rRET; typ = e0.e_type; loc = e0.e_loc } in
    expr env e0 (Some dst);
    (* less: actually should be last loc of e0 *)
    let loc = e0.e_loc in
    add_instr env (A.Instr (A5.Cmp (A5.CMP, A5.Imm 0, env.a.rRET), A5.AL)) loc;
    let pc = !(env.pc) in
    add_instr env (A.Instr (A5.Bxx (A5.EQ,(ref (A.Absolute fake_pc))),A5.AL)) loc;
    pc
  )
(*e: function [[Codegen.expr_cond]] *)

(*s: function [[Codegen.expropt]] *)
let expropt (env : 'i env) (eopt : expr option) : unit =
  match eopt with
  | None -> ()
  | Some e -> expr env e None
(*e: function [[Codegen.expropt]] *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: function [[Codegen.stmt]] *)
let rec stmt (env : 'i env) (st0 : stmt) : unit =
  match st0.s with
  (*s: [[Codegen.stmt]] match [[st0.s]] cases *)
  | Var { v_name = fullname; v_loc=_;v_storage=_;v_type=_;v_init=_iniTODO} ->
      let idinfo = Hashtbl.find env.ids_ fullname in
      (* todo: generate code for idinfo.ini, handle static locals, etc. *)

      (* update env.offsets *)
      let t = idinfo.TC.typ in
      let sizet = env.a.width_of_type {Arch_compiler.structs = env.structs_} t
      in

      (* todo: align *)
      env.offset_locals := !(env.offset_locals) + sizet;
      env.size_locals <- env.size_locals + sizet;
      Hashtbl.add env.offsets fullname !(env.offset_locals);
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
  | ExprSt e -> expr env e None
  | Block xs -> xs |> List.iter (stmt env)
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
  | Switch _ 
  | Case _ | Default _ -> 
    raise Todo
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
    add_instr env (A.Virtual (A.Jmp (ref (A.Absolute dstpc)))) st0.s_loc;
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
    add_instr env (A.Virtual (A.Jmp (ref(A.Absolute goto_for_continue)))) loc;
    patch_fake_goto env goto_for_break !(env.pc)
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
    add_instr env (A.Virtual (A.Jmp (ref(A.Absolute goto_for_continue)))) loc;
    patch_fake_goto env goto_for_break !(env.pc)
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
  | Break -> 
    (match env.break_pc with
    | Some dst ->
      add_instr env (A.Virtual (A.Jmp (ref(A.Absolute dst)))) st0.s_loc;
    | None -> raise (Impossible "should be detected in check.ml")
    )
  | Continue ->
    (match env.continue_pc with
    | Some dst ->
      add_instr env (A.Virtual (A.Jmp (ref(A.Absolute dst)))) st0.s_loc;
    | None -> raise (Impossible "should be detected in check.ml")
    )
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
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
    add_instr env (A.Virtual (A.Jmp (ref(A.Absolute goto_for_continue)))) loc;
    patch_fake_goto env goto_for_break !(env.pc)
  (*x: [[Codegen.stmt]] match [[st0.s]] cases *)
  | Return eopt ->
    (match eopt with
    | None ->
      add_instr env (A.Virtual A.RET) st0.s_loc

    | Some e ->
      (* todo: if type compatible with R0 *)
      with_reg env env.a.rRET (fun () ->
        let dst = { opd = Register env.a.rRET; typ = e.e_type; loc = e.e_loc } in
        expr env e (Some dst);

        add_instr env (A.Virtual A.RET) st0.s_loc
      )
    )
  (*e: [[Codegen.stmt]] match [[st0.s]] cases *)
(*e: function [[Codegen.stmt]] *)

(*****************************************************************************)
(* Function *)
(*****************************************************************************)
(*s: function [[Codegen.codegen_func]] *)
let codegen_func (env : 'i env) (func : func_def) : unit =
  let { f_name=name; f_loc; f_body=st; f_type=typ; f_storage=_ } = func in

  let fullname = (name, 0) in
  let idinfo = Hashtbl.find env.ids_ fullname in
  (* todo: if Flag.profile (can be disabled by #pragma) *)
  let attrs = A.default_attr in

  let spc = add_fake_instr env "TEXT" in

  (*s: [[Codegen.codegen_func()]] set [[offsets]] for parameters *)
  (* TODO: introduce helper *)
  (* set offsets for parameters *)
  let offsets = Hashtbl_.create () in

  let (_typret, (typparams, _varargs)) = typ in

  let t = idinfo.TC.typ in
  let tparams = 
    match t with
    | T.Func (_tret, tparams, _varargs) -> tparams
    | _ -> raise (Impossible "not a FUNC")
  in
  assert (List.length tparams =|= List.length typparams);

  let xs = List_.zip typparams tparams in

  let offset = ref 0 in
  xs |> List.iter (fun (p, t) ->
    let sizet = env.a.width_of_type {Arch_compiler.structs = env.structs_} t in
    p.p_name |> Option.iter (fun fullname ->
      Hashtbl.add offsets fullname !offset
    );
    (* todo: align *)
    offset := !offset + sizet;
  );
  (*e: [[Codegen.codegen_func()]] set [[offsets]] for parameters *)
  (*s: [[Codegen.codegen_func()]] adjust [[env]] *)
  (* todo: align offset_locals with return type *)
  let env = { env with
    size_locals = 0;
    size_maxargs = 0;
    offset_locals = ref 0;
    offsets       = offsets;
    labels        = Hashtbl_.create ();
    forward_gotos = Hashtbl_.create ();
    regs          = Array.copy env.a.regs_initial;
  }
  in
  (*e: [[Codegen.codegen_func()]] adjust [[env]] *)

  stmt env st;

  set_instr env spc 
    (A.Pseudo (A.TEXT (global_of_id env fullname, attrs, 
                       env.size_locals + env.size_maxargs))) f_loc;
  add_instr env (A.Virtual A.RET) f_loc;

  (*s: [[Codegen.codegen_func()]] sanity check [[env.regs]] *)
  (* sanity check register allocation *)
  env.regs |> Array.iteri (fun i v ->
    if env.a.regs_initial.(i) <> v
    then raise (Error (E.Misc (spf "reg %d left allocated" i, f_loc)));
  );
  (*e: [[Codegen.codegen_func()]] sanity check [[env.regs]] *)
  ()
(*e: function [[Codegen.codegen_func]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(*s: function [[Codegen.codegen]] *)
let codegen (arch : Arch.t) (tp : Typecheck.typed_program) : 'i Ast_asm.program =
  let env = env_of_tp arch tp in
  tp.funcs |> List.iter (codegen_func env);

  (* todo: generate code for ids after, for CGLOBAL *)
  let instrs = 
    (Array.sub !(env.code) 0 !(env.pc) |> Array.to_list) @
     List.rev !(env.data)
  in
  (* TODO *)
  let locs = [] in
  Obj.magic instrs, locs
(*e: function [[Codegen.codegen]] *)
(*e: Codegen.ml *)
