(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either
module Str = Re_str

open Ast_asm
open Ast_asmv

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mips code generation.
 *
 * The 'case <n>: ... ' comments below refer to code in vl/asm.c so one
 * can easily check the corresponding C code in vl that was used
 * as model for the OCaml code.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* Load and Store (copy pasted from Codegen5.ml) *)
type mem_opcode = LDR | STR

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error (node : 'a T.node) (s : string) =
  failwith 
    (spf "%s at %s on %s" s (T.s_of_loc node.n_loc)
        (Typesv.show_instr node.instr))
let int_of_bits (n : 'a T.node) (x : Bits.int32) : int =
  try Bits.int_of_bits32 x with
  | Failure s -> error n s

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

(* TODO: also 0x7fff, -0x8000, and 0 special cases *)
let constant_kind i =
  if i <= 0xffff
  then Some i
  else None

(* claude: BIG is the bias goken's vl gives R30 (aka SB, aka rSB
 * below) -- R30 is set up at program start to point BIG bytes into
 * the data segment, so a *later* MOVW $sym(SB) could in principle
 * reach it with one `ADD $(offset-BIG), R30, Rt` instead of loading
 * the full 32-bit absolute address (see the "lu+or" case below).
 * This is the exact same idea as ARM's R12/BIG (see Codegen5.ml's
 * offset_to_R12/immrot and docs/claude_notes/notes_arm_port_plan.txt)
 * -- except on MIPS goken's own linkers/vl/l.h sets `BIG = 0` (an
 * old value of 32766 is left commented out right above it). That
 * makes goken's actual fast-path condition,
 *   instoffset >= -BIG && instoffset < BIG && instoffset != 0
 * (span.c's aclass(), the D_ADDR/SDATA case), collapse to
 *   instoffset >= 0 && instoffset < 0
 * which no integer ever satisfies -- so on MIPS this fast path is
 * permanently dead in goken itself, not just unimplemented here.
 * That's why it was never ported: matching goken means *never*
 * taking it, for any offset, so there was nothing to port beyond
 * "always fall through to the absolute-constant load". This is
 * simpler than the ARM story, where BIG=4092 could occasionally
 * still make the fast path reachable for a large enough data
 * segment; on MIPS there is no such live case to handle.
 *)
let big = 0

let offset_to_R30 x = x - big

let base_and_offset_of_entity node symbols2 autosize x =
  match x with
  (* | Indirect (r, off) -> r, off  *)
  | (Param (_s, off)) ->
      (* remember that the +4 below is because we access the frame of the
       * caller which for sure is not a leaf. Note that autosize
       * here had possibly a +4 done if the current function
       * was a leaf, but still we need another +4 because what matters
       * now is the adjustment in the frame of the caller!
       *)
      rSP, autosize + 4 + off
  | (Local (_s, off)) -> 
      rSP, autosize + off
  | (Global (global, off)) ->
      let v = Hashtbl.find symbols2 (T.symbol_of_global global) in
      (match v with
        | T.SData2 (offset, _kind) ->
          rSB, offset_to_R30 (offset + off)
      (* stricter: allowed in 5l but I think with wrong codegen *)
      | T.SText2 _ -> 
          error node (spf "use of procedure %s in indirect with offset"
                       (A.s_of_global global))
      )


(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)
(* the functions names below are a bit cryptic but I followed the conventions
 * used in vl/asm.c (some of those names probably derives from the Mips
 * architecture manual).
 * irr: when the function take immediate register register
 * rrr: when the function take register register register
 *)

let op (x : int) (y : int) : Bits.t =
  [(x, 3); (y, 0)]

let sp (x : int) (y : int) : Bits.t =
  [(x, 29); (y, 26)]

let opirr_arith_opcode (code : arith_opcode) : Bits.t =
  match code with
  | ADD (W, S) -> sp 1 0
  | ADD (W, U) -> sp 1 1
  | ADD (V, S) -> sp 3 0
  | ADD (V, U) -> sp 3 1

  | SGT S -> sp 1 2
  | SGT U -> sp 1 3
  | AND -> sp 1 4
  | OR -> sp 1 5
  | XOR -> sp 1 6

  | SLL W -> op 0 0
  | SRL W -> op 0 2
  | SRA W -> op 0 3

  | SLL V -> op 7 0
  | SRL V -> op 7 2
  | SRA V -> op 7 3
  | _ -> failwith "TODO:opirr"

let opirr_mem (code : move2_size) (dir : mem_opcode) : Bits.t =
  match code, dir with
  | W__, STR  -> sp 5 3
  | W__, LDR -> sp 4 3
  | V__, STR -> sp 7 7
  | V__, LDR -> sp 6 7
  | F__, STR -> sp 7 1
  | F__, LDR -> sp 6 1
  | D__, _ -> failwith "TODO: opirr_mem D__ = ?"

let opirr_jmp (is_jal : bool) : Bits.t =
  if is_jal
  then sp 0 3
  else sp 0 2

(* claude: BCOND(x,y) = (x<<19)|(y<<16) in goken's asm.c -- a
 * sub-opcode selector reusing SP(0,1)'s otherwise-unused low bits to
 * distinguish BGEZ/BGEZAL/BLTZ/BLTZAL from each other (they'd
 * otherwise all share the same SP(0,1) base). *)
let bcond (x : int) (y : int) : Bits.t = [(x, 19); (y, 16)]

(* case 6's b_condition family (BGTZ/BLEZ don't need BCOND at all;
 * BGEZ/BGEZAL/BLTZ/BLTZAL do). Mirrors goken's opirr() cases for
 * these mnemonics exactly (asm.c). Paired with op_irr_no_r3, not
 * op_irr -- see its comment above. *)
let opirr_bxx_opcode (c : b_condition) : Bits.t =
  match c with
  | GEZ    -> sp 0 1 @ bcond 0 1
  | GEZAL  -> sp 0 1 @ bcond 2 1
  | GTZ    -> sp 0 7
  | LEZ    -> sp 0 6
  | LTZ    -> sp 0 1 @ bcond 0 0
  | LTZAL  -> sp 0 1 @ bcond 2 0

let oprrr_arith_opcode (code : arith_opcode) : Bits.t =
  match code with
  | ADD (W, S) -> op 4 0
  | ADD (W, U) -> op 4 1
  | ADD (V, S) -> op 5 4
  | ADD (V, U) -> op 5 5

  | SGT S -> op 5 2
  | SGT U -> op 5 3

  | AND -> op 4 4
  | OR -> op 4 5
  | XOR -> op 4 6

  | SUB (W, S) -> op 4 2
  | SUB (W, U) -> op 4 3

  | SLL W -> op 0 4
  | SRL W -> op 0 6
  | SRA W -> op 0 7

  | _ -> failwith "TODO:oprrr"

let _oprrr_mul_opcode (code : mul_opcode) : Bits.t =
  match code with
  | REM S | DIV (W, S) -> op 3 2
  | REM U | DIV (W, U) -> op 3 3
  | MUL (W, S) -> op 3 0
  | MUL (W, U) -> op 3 1
  | DIV (V, S) -> op 3 6
  | DIV (V, U) -> op 3 7

  | _ -> failwith "TODO:oprrr_mul"
  
let op_irr (op : Bits.t) (i : int) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(i land 0xffff, 0); (r2, 21); (r3, 16)]

(* claude: like op_irr but without the r3/bits[20:16] field -- needed
 * for case 6's Bxx (BGEZ/BGEZAL/BLTZ/BLTZAL) family, whose `op`
 * prefix already bakes a real value into that same bit range via
 * BCOND (see bcond/opirr_bxx_opcode below). goken's C just passes
 * p->reg == NREG there, which OP_IRR masks down to 0 anyway
 * (`&31`), so this is byte-for-byte equivalent -- but reusing plain
 * op_irr with an explicit 0 would put two entries at bit offset 16
 * in the Bits.t list, which Bits.sanity_check_32 rejects. *)
let op_irr_no_r3 (op : Bits.t) (i : int) (R r2 : reg) : Bits.t =
  op @ [(i land 0xffff, 0); (r2, 21)]

let op_rrr (op : Bits.t) (R r1 : reg) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(r1, 16); (r2, 21); (r3, 11)]

let op_jmp (op : Bits.t) (i : int) : Bits.t =
  op @ [(i land 0x3ffffff, 0)]

(* opcode to load immediate 16bits to a register
 * (ex of use: 'op_irr op_last (lcon lsr 16) rZERO rt').
 * Was called ALAST in vl where they abused this ALAST marker to
 * encode additional instructions.
 *)
let op_last = sp 1 7

(* claude: MIPS jump/branch instructions have a mandatory delay slot
 * -- the instruction right after a jump always executes too, jump
 * or not. goken's noops() (vl/noop.c) fills it with a NOP whenever
 * nothing useful can be scheduled there, and Plan9's canonical MIPS
 * NOP encoding is `NOR R0,R0,R0` (funct 0x27), not the all-zero
 * `SLL R0,R0,0` some other toolchains use -- verified against
 * goken's actual output byte-for-byte. Used below for both JMP
 * (case 18) and JAL (case 11); goken's sched.c can additionally fill
 * a *call's* delay slot with a real instruction hoisted from the
 * call target (duplicating it there) instead of a plain NOP, which
 * this doesn't replicate -- see docs/claude_notes/todo_mips_port.org.
 *)
let nop = op_rrr (op 4 7) rZERO rZERO rZERO

(*****************************************************************************)
(* More complex code generation helpers *)
(*****************************************************************************)

let gbranch_static (nsrc : 'a T.node) (is_jal : bool) : Bits.t =
  match nsrc.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst ->
      let dst_pc = ndst.real_pc in
      (* sanity check *)
      if dst_pc mod 4 <> 0
      then raise (Impossible "layout text wrong, not word aligned node");

      let v = dst_pc lsr 2 in
      op_jmp (opirr_jmp is_jal) v

(* claude: unlike gbranch_static above (JMP/JAL, an absolute word
 * address), case 6's conditional branches encode a PC-relative
 * 16-bit word displacement -- goken: `v = (p->cond->pc - pc - 4) >>
 * 2` (asm.c). The -4 accounts for the branch-delay slot: by the
 * time the branch is evaluated, pc has already advanced past the
 * delay-slot instruction that always executes right after it. *)
let gbranch_offset (nsrc : 'a T.node) : int =
  match nsrc.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst ->
      let dst_pc = ndst.real_pc in
      if dst_pc mod 4 <> 0 || nsrc.real_pc mod 4 <> 0
      then raise (Impossible "layout text wrong, not word aligned node");
      (dst_pc - nsrc.real_pc - 4) asr 2

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions:
 * - rf = register from (p->from.reg in vl)
 * - rt = register to (p->to.reg in vl)
 * - r_opt  = register middle (optional, p->reg in vl)
 *)

let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  match node.instr with
  (* Reusable *)
   | T.Virt _ | T.TEXT _ | T.WORD _ -> 
      Codegen.default_rules env init_data node
  | T.I instr ->
    (match instr with
    (* --------------------------------------------------------------------- *)
    (* Arithmetics *)
    (* --------------------------------------------------------------------- *)

    (* case 4:		/* add $scon,[r1],r2 */ *)
    | Arith (ADD (W, _sign) as op, Imm i, r_opt, rt) ->
        (* TODO: C_ADD0CON vs C_ANDCON generate different opcodes *)
        { size = 4; x = None; binary = (fun () ->
            let v = i in
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode op) v r rt ]
         ) }

    (* case 2:		/* add/sub r1,[r2],r3 */ *)
    (* claude: generic register-register-register arith; explicitly
     * excludes SLL/SRL/SRA even though oprrr_arith_opcode already
     * handles them, since goken's case 9 ("asl r1,[r2],r3") uses a
     * *different* operand order for shifts (`OP_RRR(oprrr(p->as),
     * r, p->from.reg, p->to.reg)` -- r and from.reg swapped relative
     * to this case) and case 9 isn't ported yet -- reusing this arm
     * for shifts would silently emit wrong bytes. *)
    | Arith ((ADD _ | SUB _ | AND | OR | XOR | SGT _) as op, Reg rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_rrr (oprrr_arith_opcode op) rf r rt ]
         ) }

    (* case 9:		/* asl r1,[r2],r3 */ *)
    (* claude: shift-by-register; same shape as case 2 just above,
     * but with `r` (the shift-amount register) and `rf` (the value
     * being shifted) swapped in the encoding call -- goken's
     * `OP_RRR(oprrr(p->as), r, p->from.reg, p->to.reg)` vs case 2's
     * `OP_RRR(oprrr(p->as), p->from.reg, r, p->to.reg)`. *)
    | Arith ((SLL W | SRL W | SRA W) as op, Reg rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_rrr (oprrr_arith_opcode op) r rf rt ]
         ) }

    (* case 1:		/* mov[v] r1,r2 ==> OR r1,r0,r2 */ where r1 = RO
     * which was C_ZCON case in vl span.c which was then accepted for C_REG
     * in span.c cmp() and so was matching the entry in optab.c:
     * { AMOVW,	C_REG,	C_NONE,	C_REG,		 1, 4, 0 },
     *)
    | Move2 (W__, (Right (Int 0)), Gen (GReg rt)) ->
       { size = 4; x = None; binary = (fun () ->
          [ op_rrr (oprrr_arith_opcode OR) rZERO rZERO rt ]
        ) }

    (* Constant to register move (move but no memory involved) 
     * case 3:		/* mov $soreg, r ==> or/add $i,o,r */
     *)
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) ->
       (match constant_kind i with
       | Some i -> 
           { size = 4; x = None; binary = (fun () ->
               let r = rZERO in
               (* TODO: can also be let op = OR if exactly ANDCON *)
               let op = ADD (W, U) in
               [ op_irr (opirr_arith_opcode op) i r rt ]
            ) }
       | None -> failwith "TODO: LCON"
       )

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)
    (* case 18:	/* jmp [r1],0(r2) */ *)
    | JMP { contents = (IndirectJump rt) } ->
        let r = rZERO in
        let op_jmp = op 1 0 in
        (* delay slot -- see the `nop` definition above *)
        { size = 8; x = None; binary = (fun () ->
           [ op_rrr op_jmp rZERO rt r; nop ]
         ) }
    (* case 11:	/* jmp lbra */ *)
    | JAL { contents = (Absolute _) } ->
        (* delay slot -- see the `nop` definition above. Unlike case
         * 18's RET expansion, this doesn't yet replicate goken's
         * sched.c hoisting a real instruction from the call target
         * into the slot -- functionally correct (verified: fixes a
         * real bug where the caller's next instruction was silently
         * consumed as the delay slot instead, clobbered by the
         * callee), but not byte-identical to goken's scheduled
         * output. See docs/claude_notes/todo_mips_port.org.
         *)
        { size = 8; x = None; binary = (fun () ->
          [ gbranch_static node true; nop ]
          ) }
    (* claude: same case 11 in goken's optab.c (AJMP, C_LBRA also
     * resolves to oprange 11, just without linking) -- unconditional
     * `JMP label`, as opposed to case 18's `JMP (r)` indirect form
     * just above. Added alongside case 6 below since its fixture
     * needs an unconditional jump for control flow. *)
    | JMP { contents = (Absolute _) } ->
        { size = 8; x = None; binary = (fun () ->
          [ gbranch_static node false; nop ]
          ) }

    (* case 6:	/* beq r1,[r2],sbra */ *)
    (* claude: goken's case 6 covers ABEQ/ABNE (2-register form) and
     * the whole ABGEZ/ABGEZAL/ABGTZ/ABLEZ/ABLTZ/ABLTZAL family
     * (1-register-vs-zero) uniformly with one formula, `OP_IRR(
     * opirr(p->as), v, p->from.reg, p->reg)` -- see optab.c, all
     * these mnemonics share oprange 6. Split into two match arms
     * here only because BEQ/BNE have a genuine optional middle
     * register (r_opt, defaulting to R0) while Bxx's "register"
     * slot is always the BCOND sub-opcode bits instead (see
     * op_irr_no_r3/opirr_bxx_opcode above). Same delay-slot caveat
     * as case 11 (JAL) just above: a plain nop, not goken's
     * scheduler-hoisted instruction -- see
     * docs/claude_notes/todo_mips_port.org. *)
    | BEQ (GReg rf, r_opt, _branch) ->
        { size = 8; x = None; binary = (fun () ->
            let r = r_opt ||| rZERO in
            [ op_irr (sp 0 4) (gbranch_offset node) rf r; nop ]
         ) }
    | BNE (GReg rf, r_opt, _branch) ->
        { size = 8; x = None; binary = (fun () ->
            let r = r_opt ||| rZERO in
            [ op_irr (sp 0 5) (gbranch_offset node) rf r; nop ]
         ) }
    | Bxx (cond, GReg rf, _branch) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_irr_no_r3 (opirr_bxx_opcode cond) (gbranch_offset node) rf; nop ]
         ) }

    (* --------------------------------------------------------------------- *)
    (* Memory *)
    (* --------------------------------------------------------------------- *)

    (* Address *)
    | Move2 (W__, Right ximm, Gen (GReg rt)) ->
        (match ximm with
        | Int _ | Float _ -> 
           failwith "TODO: ?? because of refactor of imm_or_ximm"
        | String _ -> 
            (* stricter? what does vl do with that? confusing I think *)
            error node "string not allowed in MOVW; use DATA"
        | Address (Global (global, _offsetTODO)) ->
              (* claude: no fast R30-relative path here -- see the
               * long comment on offset_to_R30/big above for why:
               * goken's own BIG=0 makes it permanently unreachable
               * in vl itself, so always fall through to loading the
               * full absolute address below. (offset_to_R30 is still
               * called from base_and_offset_of_entity for indirect
               * addressing -- e.g. O(R30) -- which is a different,
               * still-live code path; only the address-of-global
               * fast path here is dead.)
               *)
              (* case 19:	/* mov $lcon,r ==> lu+or */ *)
              { size = 8; x = None; binary = (fun () ->
              (* similar to WORD case *)
              (* TODO: introduce helper lcon_address_of_global *)
              let v = Hashtbl.find env.syms (T.symbol_of_global global) in
              let lcon =
                match v with
                | T.SText2 real_pc -> real_pc
                | T.SData2 (offset, _kind) -> 
                  (match init_data with
                  | None -> raise (Impossible "init_data should be set by now")
                  | Some init_data -> init_data + offset
                  )
                in
                [ op_irr op_last (lcon lsr 16) rZERO rt;
                  op_irr (opirr_arith_opcode OR) lcon rt rt;
                ]
              )}
        | Address (Local _ | Param _) -> raise Todo
        )

    (* Store/Load *)

    (* case 35:	/* mov r,lext/luto/oreg ==> sw o(r) */ *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Entity ent)) ->
        { size = 16; x = None; binary = (fun () ->
          let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
          in
          let v = offset in
          [ op_irr op_last (v lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) v rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem W__ STR) 0 rTMP rf;
          ]
          ) }
    (* case 36:	/* mov lext/lauto/lreg,r ==> lw o(r30) */ *)    
    | Move2 (W__, Left (Gen (Entity ent)), Gen (GReg rt)) ->
        { size = 16; x = None; binary = (fun () ->
            let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
            in
            let v = offset in
            [ op_irr op_last (v lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) v rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr (opirr_mem W__ LDR) 0 rTMP rt;
            ]
          ) }

    (* case 7:		/* mov r, soreg ==> sw o(r) */ *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rt, offset))) ->
        (* TODO: need look for offset if SOREG or LOREG *)
        { size = 4; x = None; binary = (fun () ->
          let r = rt in
          (* TODO: regoff *)
          let v = offset in
          [ op_irr (opirr_mem W__ STR) v r rf ]
         ) }
    (* case 8:		/* mov soreg, r ==> lw o(r) */ *)
    | Move2 (W__, Left (Gen (Indirect (rf, offset))), Gen (GReg rt)) ->
         { size = 4; x = None; binary = (fun () ->
           let r = rf in
           (* TODO: regoff *)
           let v = offset in
           [ op_irr (opirr_mem W__ LDR) v r rt ]
         ) }

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)
    (* case 5:		/* syscall */ *)
    | SYSCALL ->
       { size = 4; x = None; binary = (fun () -> [op 1 4]) }
    | BREAK ->
       { size = 4; x = None; binary = (fun () -> [op 1 5]) }

    (* --------------------------------------------------------------------- *)
    (* Other *)
    (* --------------------------------------------------------------------- *)
    |(Arith (_, _, _, R _)|NOR (_, _, _)|ArithMul (_, R _, _, R _)|ArithF _
     |Move1 (_, _, _)| Move2 _
     |RFE _|JAL _|JALReg (R _, _)|JMP _
     |BEQ (_, _, _)|BNE (_, _, _)|Bxx (_, _, _)
     |TLB _
     ) -> 
       failwith (spf "Codegenv: TODO: instr not handled: %s"
                (Typesv.show_instr node.instr))
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)


(* must return a multiple of 4 *)
let size_of_instruction  (env : Codegen.env) (node : 'a T.node) : int =
  let action  = rules env None node in
  action.size

(* TODO: could factorize parts with Codegen5.ml *)
let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in

  let pc = ref config.init_text in

  cg |> T.iter (fun n ->

    let {size; binary; x = _ }  = 
        rules Codegen.{ syms = symbols2; autosize = !autosize }
        config.init_data n 
    in
    let instrs = binary () in

    if n.real_pc <> !pc
    then raise (Impossible "Phase error, layout inconsistent with codegen");
    if List.length instrs * 4 <> size
    then raise (Impossible (spf "size of rule does not match #instrs at %s"
                              (T.s_of_loc n.n_loc)));

    let xs : Bits.int32 list = instrs |> List.map Assoc.sort_by_val_highfirst in
    
    if !Flags.debug_gen 
    then begin 
      Logs.app (fun m -> m " %.8x: %s (%s)"
                 !pc 
                  (xs |> List.map (fun x -> spf "%.8x" (int_of_bits n x))
                      |> String.concat " ")
                  (Str.global_replace (Str.regexp "[\n\t ]+") " " 
                     (Typesv.show_instr n.instr) |> String_.show_max 40));
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.debug (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
    end;

    let xs = xs |> List.map (fun x -> int_of_bits n x) in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    (* after the resolve phase the size of a TEXT is the final autosize *)
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten




