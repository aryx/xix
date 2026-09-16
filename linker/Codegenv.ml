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
(* claude: `land 0xffffffff` recovers the unsigned int this function
 * has always returned -- Int32.to_int alone sign-extends a word with
 * its top bit set (routine for machine instructions) into a negative
 * int. *)
let int_of_bits (n : 'a T.node) (x : Bits.int32) : int =
  try Int32.to_int (Bits.int_of_bits32 x) land 0xffffffff with
  | Failure s -> error n s

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

(* claude: case 3 (MOVW $con,r, no memory) covers goken's actual
 * classes ZCON/SCON/ADDCON/ANDCON -- i.e. exactly [-0x8000, 0xffff]
 * (span.c's aclass()/cmp(), same boundary reasoning as case 4/10's
 * ADD $con fix -- see that comment). The old `i <= 0xffff` check
 * was missing the lower bound entirely: any negative i trivially
 * satisfies "<= 0xffff" in OCaml, so a genuinely out-of-range value
 * like -100000 would have silently gone through the direct-fit path
 * below instead of correctly failing loudly (case 19/24's lu-based
 * expansion, not ported for a plain literal -- only for $sym(SB)
 * addresses). *)
let constant_kind i =
  if i >= -0x8000 && i <= 0xffff
  then Some i
  else None

(* claude: which single instruction case 3 uses to load i into a
 * register depends on i's *exact* class, not just whether it fits:
 * ANDCON (positive, 0x8000-0xffff) doesn't fit ADDIU's native
 * sign-extended immediate, so goken uses ORI (with source R0)
 * there instead -- using ADDIU would sign-extend 0x8000 into
 * -32768, the wrong value entirely. Everywhere else in
 * constant_kind's range (ZCON/SCON/ADDCON, [-0x8000, 0x7fff])
 * ADDIU/ADDU is correct and matches goken (confirmed via `vl -a`
 * directly: 0x7fff uses ADDIU, 0x8000/0xffff use ORI). *)
let movw_imm_opcode i =
  if i >= 0x8000
  then OR
  else ADD (W, U)

(* claude: BIG is the bias goken's vl gives R30 (aka SB, aka rSB
 * below) -- R30 is set up at program start to point BIG bytes into
 * the data segment, so a *later* MOVW $sym(SB) could in principle
 * reach it with one `ADD $(offset-BIG), R30, Rt` instead of loading
 * the full 32-bit absolute address (see the "lu+or" case below).
 * This is the exact same idea as ARM's R12/BIG (see Codegen5.ml's
 * offset_to_R12/immrot and docs/claude_notes/arm_port.md)
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
  (* claude: the +4 below used to be on Param instead of Local (and
   * Local had no adjustment at all) -- swapped after verifying
   * against goken directly (case 26's address-of-local/param, `vl
   * -a` on a fixture with a named FP/SP-relative local and param,
   * frame=$8192: goken computed offset 8192 for `x-8(FP)` (Local)
   * and 8204 for `y+8(SP)` (Param), which only matches autosize=8196
   * with the +4 on Local, not Param -- the exact same bug shape as
   * the confirmed ARM one in base_and_offset_of_indirect, see
   * docs/claude_notes/arm_port.md's case 4/34 entry). See
   * tests/linker/mips_diff/lacon_mips.s. *)
  | (Param (_s, off)) ->
      rSP, autosize + off
  | (Local (_s, off)) ->
      (* remember that the +4 below is because we access the frame of the
       * caller which for sure is not a leaf. Note that autosize
       * here had possibly a +4 done if the current function
       * was a leaf, but still we need another +4 because what matters
       * now is the adjustment in the frame of the caller!
       *)
      rSP, autosize + 4 + off
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

(* claude: the byte/halfword-sized siblings of opirr_mem above, for
 * Move1's own B_/H_ memory forms -- goken's asm.c: AMOVB/AMOVBU
 * share one STORE opcode (SB doesn't care about sign at all,
 * SP(5,0)), AMOVH/AMOVHU likewise (SP(5,1)); the LOAD side does
 * distinguish sign (LB vs LBU, LH vs LHU): SP(4,0)/SP(4,4) and
 * SP(4,1)/SP(4,5) respectively (goken's own "+ALAST" convention for
 * selecting the load-opcode variant of a row that's store by
 * default). *)
let opirr_mem1 (sz : move1_size) (dir : mem_opcode) : Bits.t =
  match sz, dir with
  | B_ _, STR -> sp 5 0
  | H_ _, STR -> sp 5 1
  | B_ A.S, LDR -> sp 4 0
  | B_ A.U, LDR -> sp 4 4
  | H_ A.S, LDR -> sp 4 1
  | H_ A.U, LDR -> sp 4 5
  | (W_ _ | V_ _), _ -> failwith "TODO: opirr_mem1 W_/V_ (MOVWL/MOVWR/MOVVL/MOVVR)"

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

(* claude: BFPT/BFPF's own base opcode -- goken's asm.c: `case ABFPT:
 * return SP(2,1)|(257<<16); case ABFPF: return SP(2,1)|(256<<16);`.
 * 257/256 sit at bits [24:16], the same "sub-opcode selector" bit
 * range BCOND uses for the other case-6 mnemonics, just a single
 * flat field here instead of two (goken's own literal already
 * combines what would otherwise be BCOND's two sub-fields). *)
(* claude: split as two sub-fields at bit24 and bit16, not one flat
 * [(257/256,16)] entry -- the latter passed type-checking but failed
 * at LINK time ("value 257 overflow outside its space (21-16)"):
 * Bits.sanity_check_32 infers each field's own width from the gap to
 * its nearest NEIGHBORING declared position in the list, not from
 * real bit 32 -- and op_irr_no_r3 (the caller) always places its own
 * `r2` field at bit21, boxing in whatever's placed at bit16 to just
 * 5 bits (21-16) -- too narrow for 257's own bit-24 contribution.
 * 257<<16 sets exactly bits 16 and 24 (257 = 0b1_0000_0001); 256<<16
 * sets only bit 24. Splitting as (1,24) + ((1|0),16) -- with bit24's
 * own neighboring gap being sp's (_,26) entry (2 bits, value 1 fits)
 * and bit16's gap being op_irr_no_r3's (_,21) entry (5 bits, value
 * 0/1 fits) -- reproduces the exact same final OR'd bit pattern
 * without straddling any single field's inferred width. A first fix
 * attempt (bcond-style split at bit19/bit16) hit the identical
 * problem one field over (32 overflowing a 2-bit 21-19 gap) before
 * this one was found by working out which bits 257/256 actually set
 * rather than guessing another 2-way split. *)
let opirr_bfp_opcode (is_true : bool) : Bits.t =
  sp 2 1 @ [(1, 24); ((if is_true then 1 else 0), 16)]

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

(* claude: only MUL(W,_) is actually reachable below (case 22) --
 * goken's own optab.c has no row at all for ADIV/AREM (nor their V
 * variants), so those mnemonics are simply unsupported by vl, not
 * just unported here. AMULU aliases the exact same oprange as AMUL
 * (span.c: `oprange[AMULU] = oprange[r]`), matching MUL(W,U) below. *)
let oprrr_mul_opcode (code : mul_opcode) : Bits.t =
  match code with
  | REM S | DIV (W, S) -> op 3 2
  | REM U | DIV (W, U) -> op 3 3
  | MUL (W, S) -> op 3 0
  | MUL (W, U) -> op 3 1
  | DIV (V, S) -> op 3 6
  | DIV (V, U) -> op 3 7

  | _ -> failwith "TODO:oprrr_mul"

(* claude: FPF(x,y) = SP(2,1)|(16<<21)|(x<<3)|y and FPD(x,y) is the
 * same with 17 instead of 16 -- goken's asm.c macros selecting
 * single- vs double-precision floating point ops (bit 21's value is
 * the "fmt" field in the real MIPS FPU encoding: 16=single,
 * 17=double). *)
let fpf (x : int) (y : int) : Bits.t = sp 2 1 @ [(16, 21)] @ op x y
let fpd (x : int) (y : int) : Bits.t = sp 2 1 @ [(17, 21)] @ op x y
(* claude: FPW(x,y) is the same shape with 20 instead of 16/17 --
 * goken's own macro (`#define FPW(x,y) SP(2,1)|(20<<21)|(x<<3)|y`),
 * the "fmt" field value for the word-integer source/dest format used
 * by the MOVWF/MOVWD conversion pair -- see FCvt's own comment. *)
let fpw (x : int) (y : int) : Bits.t = sp 2 1 @ [(20, 21)] @ op x y

(* claude: goken's own asm.c case-46-ish opcode table for the
 * word<->float<->double conversion family (see Ast_asmv.ml's own
 * fcvt_dir comment for the exact FPx(a,b) per direction, individually
 * verified against asm.c, not re-derived here). Shares case 33's
 * exact encoding shape with ABS_/NEG_ above (OP_FRRR(op,0,from,to)),
 * confirmed via goken's own span.c buildrep() mechanism replicating
 * AMOVF/AMOVD's own case-33 optab row onto this whole family. *)
let oprrr_fcvt_opcode (dir : fcvt_dir) : Bits.t =
  match dir with
  | FW -> fpf 4 4
  | DW -> fpd 4 4
  | WF -> fpw 4 0
  | DF -> fpd 4 0
  | WD -> fpw 4 1
  | FD -> fpf 4 1

(* claude: ADD_/SUB_/MUL_/DIV_ (case 32), ABS_/NEG_ (case 33), and
 * CMPEQ_/CMPGT_/CMPGE_ (also case 32 -- see that dispatch arm's own
 * comment for why the C_REG-not-C_FREG optab quirk doesn't actually
 * matter here) -- goken's own asm.c: ACMPEQF=FPF(6,2),
 * ACMPGTF=FPF(7,4), ACMPGEF=FPF(7,6) (and the D-precision siblings
 * at the same (x,y) pair, just fpd instead of fpf). *)
let oprrr_arithf_opcode ((code, prec) : arithf_opcode * A.floatp_precision) : Bits.t =
  let f = match prec with A.F -> fpf | A.D -> fpd in
  match code with
  | ADD_ -> f 0 0
  | SUB_ -> f 0 1
  | MUL_ -> f 0 2
  | DIV_ -> f 0 3
  | ABS_ -> f 0 5
  | NEG_ -> f 0 7
  | CMPEQ_ -> f 6 2
  | CMPGT_ -> f 7 4
  | CMPGE_ -> f 7 6

let op_frrr (op : Bits.t) (FR r1 : freg) (FR r2 : freg) (FR r3 : freg) : Bits.t =
  op @ [(r1, 16); (r2, 11); (r3, 6)]

(* claude: OP_RRR(SP(2,1)|(4<<21), rint, 0, rfloat) for MTC1 (case
 * 30, is_mtc1=true) and OP_RRR(SP(2,1)|(0<<21), rint, 0, rfloat)
 * for MFC1 (case 31, is_mtc1=false) -- the 4-vs-0 sub-field (bits
 * 21-25) selects the direction; goken's C always passes 0 for the
 * "r2" slot there, which per OP_RRR's own layout is really just
 * this same sub-field, not an actual middle operand -- hence baking
 * it directly into the opcode prefix here instead of a separate
 * Bits.t tuple. *)
let op_mfc_mtc (is_mtc1 : bool) (r_int : int) (r_float : int) : Bits.t =
  sp 2 1 @ [((if is_mtc1 then 4 else 0), 21); (r_int, 16); (r_float, 11)]

(* claude: same shape as op_mfc_mtc, but SP(2,0) (coprocessor-0, not
 * -1) and a size-dependent sub-field: case 37 (MTC0/DMTC0) uses 4/5,
 * case 38 (MFC0/DMFC0) uses 0/1 -- W__ vs V__. *)
let op_mc0 (sub : int) (r_int : int) (r_m : int) : Bits.t =
  sp 2 0 @ [(sub, 21); (r_int, 16); (r_m, 11)]

(* claude: MFCC1/MTCC1 (case 41/42, moves to/from an FCR -- a
 * *control* register, not a data one like MTC1/MFC1 above, hence
 * the different sub-field within the same SP(2,1) family: 2 for
 * MFCC1 (read), 6 for MTCC1 (write). *)
let op_cfc_ctc (is_ctc1 : bool) (r_int : int) (r_fcr : int) : Bits.t =
  sp 2 1 @ [((if is_ctc1 then 6 else 2), 21); (r_int, 16); (r_fcr, 11)]

let op_irr (op : Bits.t) (i : int) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(i land 0xffff, 0); (r2, 21); (r3, 16)]

(* claude: like op_irr but taking plain ints instead of a typed reg
 * for r3 -- needed for case 27/28 (LWC1/SWC1), where r3 is a float
 * register (freg), not an int one, but occupies the exact same
 * bit field. *)
let op_irr_raw (op : Bits.t) (i : int) (r2 : int) (r3 : int) : Bits.t =
  op @ [(i land 0xffff, 0); (r2, 21); (r3, 16)]

(* claude: OP_SRR(op,s,r2,r3) in goken's asm.c -- shift-immediate
 * form, used by case 12's SLL/SRA-based sign-extend trick below
 * (case 16, sll $c,[r1],r2, would also use this but isn't ported
 * yet). *)
let op_srr (op : Bits.t) (s : int) (R r2 : reg) (R r3 : reg) : Bits.t =
  op @ [(s land 0x1f, 6); (r2, 16); (r3, 11)]

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
 * this doesn't replicate -- see docs/claude_notes/mips_port.md.
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
    (* claude: goken's case 4 (needed class C_ADD0CON) is reached
     * whenever the constant fits directly in ADDI/ADDIU's native
     * sign-extended 16-bit immediate, i.e. actual class ZCON, SCON
     * or ADDCON (span.c's cmp(): C_ADD0CON falls through to
     * C_ADDCON's own C_ZCON/C_SCON acceptance) -- confirmed
     * empirically against goken directly (`vl -a`) for the full
     * [-0x8000, 0x7fff] range, single instruction both ends.
     * Outside that range needs case 10's REGTMP+OR trick (ANDCON,
     * 0x8000-0xffff) below, or cases 25/23 further below still
     * (UCON/LCON) -- this guard used to silently truncate any
     * out-of-range value via `i land 0xffff` instead, a real,
     * previously-latent bug for e.g. ADD $0x8000,R1,R2 (would have
     * encoded as ADDI $-0x8000 instead of REGTMP-based +0x8000).
     *)
    | Arith (ADD (W, _sign) as op, Imm i, r_opt, rt) when i >= -0x8000 && i <= 0x7fff ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode op) i r rt ]
         ) }

    (* case 4 sibling: SGT/SGTU $con,[r1],r2 -- real hardware
     * SLTI/SLTIU (opirr_arith_opcode's own SGT rows, `sp 1 2`/`sp 1
     * 3`, already decode to the real 0x0a/0x0b SLTI/SLTIU opcodes,
     * confirmed by manually decoding a real goken-linked binary's
     * raw words for "SGT $128,R11,R2" -> SLTI R2,R11,128, no operand
     * swap despite the "greater than" name) -- same single-
     * instruction 16-bit-signed-immediate range as ADD's own case 4
     * just above, kept as its own guarded arm rather than widening
     * ADD's (goken's own ADDCON/ANDCON/UCON/LCON fallback chain for
     * out-of-range ADD immediates isn't verified to apply identically
     * to SGT, so only the range actually proven needed is ported).
     * Found stress-testing real lib_core/libc (utf/rune.c's real
     * "SGT $128,R11,R2"). *)
    | Arith (SGT _ as op, Imm i, r_opt, rt) when i >= -0x8000 && i <= 0x7fff ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode op) i r rt ]
         ) }

    (* claude: NOT byte-identical to goken here (same "real literal
     * pool this port doesn't implement" gap as the MOVW $lcon,R/
     * AND-OR-XOR-with-a-huge-constant cases elsewhere in this file):
     * SGT/SGTU with a constant too big for SLTI/SLTIU's own signed
     * 16-bit immediate (e.g. a real "SGT $4294967292,R10,R2", i.e.
     * -4 as a 32-bit pattern). Same REGTMP-materialize-then-
     * register-op substitute as those other cases -- genuinely
     * correct, just not a claim of real `va` byte parity. Found
     * stress-testing real lib_core/libc (fmt/dofmt.c's real "SGT
     * $4294967292,R10,R2"). *)
    | Arith (SGT _ as op, Imm i, r_opt, rt) ->
        { size = 12; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr op_last (i asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) (i land 0xffff) rTMP rTMP;
              op_rrr (oprrr_arith_opcode op) rTMP r rt;
            ]
         ) }

    (* case 4 sibling: AND/OR/XOR $con,[r1],r2 -- real hardware
     * ANDI/ORI/XORI, whose immediate is zero- not sign-extended
     * (goken's own optab.c: `{AAND, C_AND0CON, ...}` uses this same
     * case 4, gated on its own C_AND0CON class rather than ADD's
     * C_ADD0CON -- real ANDI/ORI/XORI take a plain 16-bit unsigned
     * immediate, [0,0xffff], not ADD's signed [-0x8000,0x7fff]).
     * opirr_arith_opcode's own AND/OR/XOR rows already existed
     * (`sp 1 4`/`sp 1 5`/`sp 1 6`); only this dispatch arm was
     * missing. Found stress-testing real lib_core/libc (utf/rune.c's
     * real "AND $1,R3", fmt/dofmt.c's real "OR $256,R9,R2"). *)
    | Arith ((AND | OR | XOR) as op, Imm i, r_opt, rt) when i >= 0 && i <= 0xffff ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode op) i r rt ]
         ) }

    (* claude: NOT byte-identical to goken here (same "real literal
     * pool this port doesn't implement" gap as the general MOVW
     * $lcon,R case above): AND/OR/XOR with a constant too big for
     * ANDI/ORI/XORI's own 16-bit unsigned immediate (e.g. a real
     * "AND $4294967247,R2,R9", i.e. 0xFFFFFFCF/-49 as a 32-bit
     * pattern) needs its own real literal-pool mechanism in the
     * assembler this port doesn't have. Deliberate xix-only
     * substitute instead: materialize the constant into REGTMP via
     * the exact same mathematically-exact LUI+ORI expansion the
     * MOVW fallback above already uses, then the already-working
     * register-register form of the same op -- genuinely correct,
     * just not a claim of real `va` byte parity. Found stress-
     * testing real lib_core/libc (fmt/dofmt.c's real "AND
     * $4294967247,R2,R9"). *)
    | Arith ((AND | OR | XOR) as op, Imm i, r_opt, rt) ->
        { size = 12; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr op_last (i asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) (i land 0xffff) rTMP rTMP;
              op_rrr (oprrr_arith_opcode op) rTMP r rt;
            ]
         ) }

    (* claude: "NOR $imm,Rd" -- a real goken 2-operand pseudo-op
     * (Parser_asmv.mly's own `TNOR imr TC imr` production), computing
     * Rd = ~(Rd | imm) in place -- MIPS's own idiom for bitwise NOT
     * when imm=0 (there's no separate hardware "NOT"; confirmed
     * directly against goken with "NOR $0,R1" on R1=5 -> R1=~5=250 as
     * an exit code, and by decoding the real linked instruction word:
     * a plain NOR with rs=R1, rt=R0, rd=R1). Real MIPS has no NORI
     * (immediate NOR) hardware instruction either, so a nonzero imm
     * needs the same REGTMP-materialize substitute as the AND/OR/XOR
     * large-immediate case above; imm=0 (the only case any real
     * closure stress-tested so far needs) skips that and uses rZERO
     * directly, matching goken's own real bytes exactly. Found
     * stress-testing real lib_core/libc (fmt/nan64.c's real "NOR
     * $0,R0", clearing/complementing a flag register). *)
    | NOR (Imm 0, r_opt, Reg rd) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rd in
            [ op_rrr (op 4 7) rZERO r rd ]
         ) }
    | NOR (Imm i, r_opt, Reg rd) ->
        { size = 12; x = None; binary = (fun () ->
            let r = r_opt ||| rd in
            [ op_irr op_last (i asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) (i land 0xffff) rTMP rTMP;
              op_rrr (op 4 7) rTMP r rd;
            ]
         ) }

    (* case 10:	/* add $con,[r1],r2 ==> mov $con,t; add t,[r1],r2 */ *)
    (* claude: the ANDCON range (0x8000-0xffff -- positive, doesn't
     * fit ADDI's signed immediate, but does fit a 16-bit OR-with-R0
     * load). goken's C also has an AADDU/sign-extend variant for
     * negative values here (`if(v<0) r=AADDU`), but that path is
     * only ever reached for AAND (ADD's own negative case is fully
     * covered by case 4 above, per the ADDCON note there) --
     * AND-immediate isn't ported at all yet (case 4 above is
     * ADD-only), so this only implements the OR/positive half.
     *)
    | Arith (ADD (W, _sign) as op, Imm i, r_opt, rt) when i >= 0x8000 && i <= 0xffff ->
        { size = 8; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr (opirr_arith_opcode OR) i rZERO rTMP;
              op_rrr (oprrr_arith_opcode op) rTMP r rt ]
         ) }

    (* case 25:	/* add/and $ucon,[r1],r2 ==> lu $con,t; add t,[r1],r2 */ *)
    (* claude: UCON (low 16 bits all zero, magnitude beyond case
     * 10's ANDCON range) -- LU into REGTMP (no OR needed, no low
     * bits to merge), then the real op. Unlike MOVW's case 19,
     * ADD's immediate forms do NOT hit the literal-pool surprise
     * documented on case 24/19 above -- confirmed via `vl -a`
     * directly (`ADD $65536,R1,R2` is the plain 2-instruction
     * LU+ADD shown here, no symbol reference). *)
    | Arith (ADD (W, _sign) as op, Imm i, r_opt, rt) when i land 0xffff = 0 ->
        { size = 8; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr op_last (i asr 16) rZERO rTMP;
              op_rrr (oprrr_arith_opcode op) rTMP r rt ]
         ) }

    (* case 23:	/* add $lcon,r1,r2 ==> lu+or+add */ *)
    (* claude: the fallback for any i that's neither ADD0CON/SCON/
     * ANDCON (case 4/10) nor UCON (case 25) -- a genuine 32-bit
     * constant, needing both halves loaded into REGTMP before the
     * real op. Also confirmed via `vl -a` to be the plain
     * 3-instruction LU+OR+ADD, no literal-pool surprise (unlike
     * MOVW's case 19). goken's C also diags if p->to.reg or p->reg
     * is REGTMP itself ("cant synthesize large constant") -- not
     * replicated, since it's a real assembler-error case that no
     * fixture exercises, not a byte-matching concern. *)
    | Arith (ADD (W, _sign) as op, Imm i, r_opt, rt) ->
        { size = 12; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_irr op_last (i asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) i rTMP rTMP;
              op_rrr (oprrr_arith_opcode op) rTMP r rt ]
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

    (* case 16:	/* sll $c,[r1],r2 */ *)
    (* claude: shift-by-immediate; goken's C also has a >=32-shift
     * ALAST-aliased path for the vshift() opcodes (SLLV/SRLV/SRAV,
     * AST's V-sized shifts), not implemented here -- same scoping
     * choice as case 9 just above, which also only covers W-sized
     * shifts. A W-sized shift count is always < 32 in practice
     * (undefined otherwise on real MIPS hardware), so this always
     * takes goken's plain (non-ALAST) OP_SRR branch. *)
    | Arith ((SLL W | SRL W | SRA W) as op, Imm v, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_srr (opirr_arith_opcode op) v r rt ]
         ) }

    (* case 22:	/* mul r1,r2 */ *)
    (* claude: real MIPS MULT has no destination register field at
     * all (the result always lands in HI/LO, retrieved separately
     * via case 20's MFHI/MFLO) -- goken's optab.c row for AMUL has
     * "to" declared C_NONE, so REGZERO fills that encoding slot
     * unconditionally. xix's own grammar (Parser_asmv.mly) parses
     * the 2-operand form "MUL R1,R2" into ArithMul(MUL,R1,None,R2)
     * (dest=R2, no r_opt) rather than into r_opt -- but `r_opt|||rt`
     * recovers the same R2 either way, landing on the identical
     * encoding as goken's own parser (confirmed via `vl -a`
     * directly: R1 ends up in the rt field, R2 in rs, matching
     * OP_RRR(op, p->from.reg=R1, p->reg=R2, REGZERO) exactly). *)
    | ArithMul ((MUL (W, _) as op), rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_rrr (oprrr_mul_opcode op) rf r rZERO ]
         ) }
    (* case 22 sibling: DIV/DIVU -- same shape as MUL just above (real
     * MIPS DIV also has no destination register field, result lands
     * in HI/LO, retrieved separately via MFHI/MFLO). Found stress-
     * testing real lib_core/libc (fmt/dofmt.c's real "DIV R4,R9"). *)
    | ArithMul ((DIV (W, _) as op), rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_rrr (oprrr_mul_opcode op) rf r rZERO ]
         ) }

    (* case 30:	/* movw r,fr */ *)
    (* claude: MTC1/MFC1 (case 30/31) have a mandatory MIPS I COP1
     * transfer delay slot, just like a branch -- confirmed via
     * `vl -a`: an isolated MTC1/MFC1 right before SYSCALL gets a
     * plain NOR/NOP padded after it (nothing eligible to hoist
     * there), and when something IS eligible, goken's sched.c fills
     * the slot with a real hoisted instruction instead (it also
     * specifically avoids putting a second COP1 transfer in that
     * slot -- confirmed empirically, not fully characterized). Same
     * already-documented, out-of-scope scheduler gap as every
     * branch/call delay slot and the MUL/HI-LO hazard this session
     * -- this always emits a plain nop, not a hoisted instruction. *)
    | Move2 (W__, Left (Gen (GReg (R rf))), GFReg (FR rt)) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_mfc_mtc true rf rt; nop ]
         ) }

    (* case 31:	/* movw fr,r */ *)
    | Move2 (W__, Left (GFReg (FR rf)), Gen (GReg (R rt))) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_mfc_mtc false rt rf; nop ]
         ) }

    (* case 37:	/* movw r,mr */ *)
    (* claude: MTC0/DMTC0 (coprocessor-0/MMU register write) -- no
     * delay slot needed here, unlike case 38's read below (confirmed
     * via `vl -a`: a write immediately followed by a read of the
     * *same* M register needs no padding between them at all). *)
    | Move2 (W__, Left (Gen (GReg (R rf))), MReg (M rt)) ->
        { size = 4; x = None; binary = (fun () -> [ op_mc0 4 rf rt ]) }
    | Move2 (V__, Left (Gen (GReg (R rf))), MReg (M rt)) ->
        { size = 4; x = None; binary = (fun () -> [ op_mc0 5 rf rt ]) }

    (* case 38:	/* movw mr,r */ *)
    (* claude: MFC0/DMFC0 -- goken's noop.c has a dedicated 2-NOP
     * special case for any AMOVW/AMOVV whose *source* is D_MREG or
     * D_FCREG (`if(p->from.type==D_FCREG||D_MREG){addnop(p);
     * addnop(p);}`), distinct from the COP1/float and plain-load
     * 1-NOP hazards elsewhere this session. Confirmed via `vl -a`
     * for an isolated MFC0 (2 NOPs even with an unrelated eligible
     * instruction right after -- unlike sched()'s usual hoisting,
     * this specific pair is inserted directly in noop.c's own first
     * pass), but NOT fully characterized when *another* MREG/FCREG
     * instruction sits nearby (one combined test showed only one of
     * two chained reads getting padded) -- this always emits 2 nops
     * unconditionally, matching the isolated case; a fixture
     * chaining multiple M-register ops close together may not be
     * byte-identical. *)
    | Move2 (W__, Left (MReg (M rf)), Gen (GReg (R rt))) ->
        { size = 12; x = None; binary = (fun () -> [ op_mc0 0 rt rf; nop; nop ]) }
    | Move2 (V__, Left (MReg (M rf)), Gen (GReg (R rt))) ->
        { size = 12; x = None; binary = (fun () -> [ op_mc0 1 rt rf; nop; nop ]) }

    (* case 41:	/* movw r,fcr */ *)
    (* claude: MTCC1 (write) -- goken's C does a dummy MFCC1 read
     * into REGZERO first (`OP_RRR(SP(2,1)|(2<<21), REGZERO, 0,
     * p->to.reg)`), a real MIPS I FCR hazard workaround, before the
     * actual MTCC1 write; no delay slot needed after (confirmed via
     * `vl -a`). *)
    | Move2 (W__, Left (Gen (GReg (R rf))), FCReg (FCR rt)) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_cfc_ctc false 0 rt; op_cfc_ctc true rf rt ]
         ) }

    (* case 42:	/* movw fcr,r */ *)
    (* claude: MFCC1 (read) -- single instruction, but like case 38
     * (MFC0) this is a D_FCREG source, so it gets the same
     * unconditional 2-NOP special case from noop.c (see case 38's
     * comment) rather than the 1-NOP COP1/load hazards elsewhere
     * this session. *)
    | Move2 (W__, Left (FCReg (FCR rf)), Gen (GReg (R rt))) ->
        { size = 12; x = None; binary = (fun () ->
            [ op_cfc_ctc false rt rf; nop; nop ]
         ) }

    (* case 32:	/* fadd fr1,[fr2],fr3 */ *)
    | ArithF (((ADD_ | SUB_ | MUL_ | DIV_), _) as op, rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_frrr (oprrr_arithf_opcode op) rf r rt ]
         ) }

    (* case 32, compare variant: CMPEQ_/CMPGT_/CMPGE_ -- a REAL bug
     * this session's first attempt got wrong, caught only by
     * decoding real goken's own raw instruction words (not by
     * reading asm.c's C source alone, which looked like it should
     * "just work" via the same OP_FRRR(op,from,to,to) call every
     * other case-32 op uses): goken's own C really does pass
     * `p->to.reg` as OP_FRRR's third argument for a compare too, but
     * a real MIPS FP *compare* has no third 5-bit register field at
     * all in that bit position (bits[10:6]) -- that range is the
     * condition-code selector (cc, bits[10:8]) plus 2 reserved bits,
     * always 0 for the plain "cc0" form this port uses, NOT a
     * register. Confirmed by manually decoding "CMPEQD F2,F4"
     * linked with real goken: ft=2(F2), fs=4(F4), bits[10:6]=0 --
     * NOT to.reg(=4) shifted into that slot, which is what op_frrr's
     * generic 3-register shape (reused unmodified from the earlier,
     * wrong attempt) actually produced, corrupting the encoding by
     * exactly bit 8. Found the hard way: the closure linked and even
     * *ran*, but silently computed the wrong CMPEQD/CMPGED condition
     * result, discovered only via a hand-written CMPEQD+BFPF probe
     * after a real closure-wide `%d`-formatting bug (a completely
     * different, already-fixed issue) was ruled out first. *)
    | ArithF (((CMPEQ_ | CMPGE_ | CMPGT_), _) as op, rf, r_opt, rt) ->
        { size = 4; x = None; binary = (fun () ->
            let r = r_opt ||| rt in
            [ op_frrr (oprrr_arithf_opcode op) rf r (FR 0) ]
         ) }

    (* case 33:	/* fabs fr1,fr3 */ *)
    (* claude: unary -- goken's C always fills the ft field with 0
     * (unused for a 2-operand op), and per the AST/grammar these
     * never take a middle register (ArithF's r_opt is always None
     * for ABS_/NEG_ -- see the AST comment on arithf_opcode). *)
    | ArithF (((ABS_ | NEG_), _) as op, rf, None, rt) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_frrr (oprrr_arithf_opcode op) (FR 0) rf rt ]
         ) }

    (* case 33, plain register move variant: "MOVF F1,F2"/"MOVD F1,F2"
     * -- goken's own real optab.c rows (`{AMOVF, C_FREG, C_NONE,
     * C_FREG, 33, ...}` / same for AMOVD) share this exact case with
     * ABS_/NEG_ just above (`oprrr(AMOVF)=FPF(0,6)`,
     * `oprrr(AMOVD)=FPD(0,6)`), just a genuine copy instead of a
     * unary op. Already reachable via the existing Move2/vgen
     * grammar (freg is already a real `vgen` alternative) -- no new
     * AST constructor or grammar rule needed, only this dispatch
     * arm. Found stress-testing real lib_core/libc (fmt/strtod.c's
     * real "MOVD F4,F0"). *)
    | Move2 (F__, Left (GFReg (FR fsrc)), GFReg (FR rt)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_frrr (fpf 0 6) (FR 0) (FR fsrc) (FR rt) ]
         ) }
    | Move2 (D__, Left (GFReg (FR fsrc)), GFReg (FR rt)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_frrr (fpd 0 6) (FR 0) (FR fsrc) (FR rt) ]
         ) }

    (* case 33, conversion variant: MOVWD/MOVDW/MOVWF/MOVFW/MOVDF/MOVFD
     * -- same shape as ABS_/NEG_ just above (see FCvt's own comment
     * for why this isn't just another arithf_opcode). *)
    | FCvt (dir, rf, rt) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_frrr (oprrr_fcvt_opcode dir) (FR 0) rf rt ]
         ) }

    (* case 20:	/* mov lohi,r */ *)
    (* claude: MFHI/MFLO -- reads the implicit HI/LO result register
     * (see case 22 above) into a real register. goken's C special-
     * cases on `p->from.type == D_LO` to flip between OP(2,0) and
     * OP(2,2); here that's just direct pattern matching on the
     * lohireg. Note: goken's *scheduler* (sched.c) sometimes pads
     * two NOPs around a MUL result being read via MFHI/MFLO (a real
     * MIPS I HI/LO hazard) -- confirmed empirically via `vl -a`, but
     * the exact trigger isn't fully characterized (it's not simply
     * "immediately after the MUL": some adjacent arrangements need
     * no padding while some buffered ones do -- see
     * docs/claude_notes/mips_port.md). Not ported, same
     * already-documented reason as every other sched.c gap this
     * session (branch/call delay slots) -- a fixture chaining MUL
     * into MFHI/MFLO will be functionally correct but not
     * necessarily byte-identical. *)
    | Move2 (W__, Left (LoHi LO), Gen (GReg rt)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_rrr (op 2 2) rZERO rZERO rt ]
         ) }
    | Move2 (W__, Left (LoHi HI), Gen (GReg rt)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_rrr (op 2 0) rZERO rZERO rt ]
         ) }

    (* case 1: mov[v] r1,r2 ==> OR r1,r0,r2 *)
    (* claude: plain register-to-register move -- no dedicated MIPS
     * hardware "move" instruction, goken's own real case 1 synthesizes
     * it as "OR r1,R0,r2" (goken's `OP_RRR(oprrr(AOR), p->from.reg,
     * REGZERO, p->to.reg)`). Found stress-testing real lib_core/libc
     * (fmt/nan64.c's real "MOVW R6,R4"). *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (GReg rt)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_rrr (oprrr_arith_opcode OR) rf rZERO rt ]
         ) }

    (* case 21:	/* mov r,lohi */ *)
    (* claude: MTHI/MTLO -- the reverse of case 20, writing a real
     * register's value into the implicit HI or LO register. *)
    | Move2 (W__, Left (Gen (GReg rf)), LoHi LO) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_rrr (op 2 3) rZERO rf rZERO ]
         ) }
    | Move2 (W__, Left (Gen (GReg rf)), LoHi HI) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_rrr (op 2 1) rZERO rf rZERO ]
         ) }

    (* case 12:	/* movbs r,r */ *)
    (* claude: sign-extending byte/half register move, done with no
     * dedicated instruction -- goken shifts left then arithmetic-
     * shifts right by the same amount (24 for a byte, 16 for a
     * half), which pushes the sign bit up to bit 31 and then
     * sign-extends it back down. *)
    | Move1 (B_ S, Left (GReg rf), GReg rt) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_srr (opirr_arith_opcode (SLL W)) 24 rf rt;
              op_srr (opirr_arith_opcode (SRA W)) 24 rt rt ]
         ) }
    | Move1 (H_ S, Left (GReg rf), GReg rt) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_srr (opirr_arith_opcode (SLL W)) 16 rf rt;
              op_srr (opirr_arith_opcode (SRA W)) 16 rt rt ]
         ) }

    (* case 13:	/* movbu r,r */ *)
    (* claude: zero-extending byte/half register move -- just a
     * plain AND-immediate mask (0xff or 0xffff), no shifting
     * needed. *)
    | Move1 (B_ U, Left (GReg rf), GReg rt) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_irr (opirr_arith_opcode AND) 0xff rf rt ]
         ) }
    | Move1 (H_ U, Left (GReg rf), GReg rt) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_irr (opirr_arith_opcode AND) 0xffff rf rt ]
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
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) when constant_kind i <> None ->
       { size = 4; x = None; binary = (fun () ->
           let r = rZERO in
           let op = movw_imm_opcode i in
           [ op_irr (opirr_arith_opcode op) i r rt ]
        ) }

    (* case 24:	/* mov $ucon,,r ==> lu r */ *)
    (* claude: UCON (low 16 bits all zero, magnitude outside case
     * 3's range above) -- a single LUI, no OR needed since there
     * are no low bits to merge in. `i asr 16` (not `lsr`) to match
     * goken's C `v>>16` on a signed value -- irrelevant to the
     * final encoding (op_irr masks with `land 0xffff` anyway) but
     * keeps the intermediate value's sign consistent with goken's. *)
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) when i land 0xffff = 0 ->
       { size = 4; x = None; binary = (fun () ->
           [ op_irr op_last (i asr 16) rZERO rt ]
        ) }

    (* claude: NOT byte-identical to goken here (case 19's real
     * mechanism, kept for real reference): a genuine LCON *literal*
     * (nonzero low 16 bits, magnitude beyond case 10's ANDCON range)
     * makes goken's `va` rewrite the MOVW into a completely
     * different 4-instruction sequence that loads the constant's
     * *value* from a synthesized SB-relative data symbol (a literal
     * pool, analogous to ARM's -- see Layout5.ml/docs/claude_notes/
     * arm_port.md's pool-dedup TODO) -- e.g. `MOVW $305419896,R1`
     * assembles to LUI+ORI+ADD+LW against a symbol literally named
     * "12345678(SB)", not a plain LU+OR. That's a genuine
     * *assembler*-side mechanism (`va`/`ova`, a real MIPS literal
     * pool) this port doesn't implement. The plain LUI+ORI expansion
     * below IS still mathematically exact for any 32-bit value
     * (`((i asr 16) land 0xffff) lsl 16 | (i land 0xffff) = i`,
     * unconditionally) -- only the *bytes* differ from goken's own
     * pool-based approach, not the resulting register value -- so
     * this is a deliberate xix-only substitute (same category as
     * every other REGTMP-materialize deviation this session),
     * genuinely correct, just not a claim of real `va` byte parity.
     * Found stress-testing real lib_core/libc (fmt/strtod.c's real
     * "MOVW $1048575,R2" and many siblings). *)
    | Move2 (W__, (Right (Int i)), Gen (GReg rt)) ->
       { size = 8; x = None; binary = (fun () ->
           [ op_irr op_last (i asr 16) rZERO rt;
             op_irr (opirr_arith_opcode OR) (i land 0xffff) rt rt;
           ]
        ) }

    (* case 34:	/* mov $con,fr ==> or/add $i,r,r2 */ *)
    (* claude: float-constant load -- same OR-vs-ADDU choice as case
     * 3 (reuses movw_imm_opcode), just landing in REGTMP first and
     * then MTC1'd into the float register instead of writing an int
     * register directly. goken's optab.c only lists ADDCON/ANDCON
     * for this case (same [-0x8000,0xffff] range as case 3, via
     * constant_kind) -- no UCON/LCON variant here; those instead
     * reuse case 35/36's oprange (per optab.c), which already have
     * a different, integer-register-specific implementation below,
     * so porting the float UCON/LCON variants needs its own look at
     * whether that's really shared or a distinct sub-case, left as
     * a TODO rather than guessed at. Unlike case 30/31's standalone
     * MTC1, this does NOT get a trailing nop -- confirmed via `vl
     * -a` directly (a bare `MOVW $42,F0` right before SYSCALL is
     * just the 2 words below, no NOR/NOP after). Whatever exempts it
     * (goken's delay-slot marking likely keys off the *original*,
     * pre-expansion instruction/Prog, not each emitted word) isn't
     * fully understood, but if something immediately reads the
     * float register afterward (e.g. a following MFC1), *that*
     * instruction's own mandatory delay slot (case 31's nop) still
     * applies and produces the right total byte count. *)
    | Move2 (W__, (Right (Int i)), GFReg (FR rt)) when constant_kind i <> None ->
        { size = 8; x = None; binary = (fun () ->
            let op = movw_imm_opcode i in
            let (R rtmp) = rTMP in
            [ op_irr (opirr_arith_opcode op) i rZERO rTMP;
              op_mfc_mtc true rtmp rt ]
         ) }

    (* case 34, D__ variant: a genuine double-precision float
     * *literal* ("MOVD $0.5,F2"), not an int bit-pattern reload --
     * same "no native 64-bit FPU register-immediate load" story as
     * case 27/28's own D__ memory forms above: split the constant's
     * raw IEEE754 bit pattern (Int64.bits_of_float) into its two
     * 32-bit halves and MTC1 each into the register pair separately.
     * Register assignment (low half -> Fn, high half -> Fn+1) matches
     * the ABI convention the memory-load case's own address-based
     * ordering already implied (big-endian: low address holds the
     * MSW, and that address mapped to Fn+1 there -- so Fn+1 = MSW =
     * high half, Fn = LSW = low half, consistently). xix-only
     * expansion (no `constant_kind`-style range restriction at all,
     * unlike the W__/int-reload case just above, since an arbitrary
     * double's bit pattern essentially never fits a narrow range) --
     * not a claim of real `va` byte parity (goken's own chipfloat-
     * style narrow immediate path, if any, isn't replicated). Found
     * stress-testing real lib_core/libc (fmt/strtod.c's real "MOVD
     * $0.5,F2" -- one of the 8 chipfloat-style constants elsewhere
     * in this pipeline, but genuinely still a bare float literal
     * here, not routed through any int-reload path). *)
    | Move2 (D__, Right (Float f), GFReg (FR rt)) ->
        { size = 24; x = None; binary = (fun () ->
            (* claude: recent OCaml would just do:
             *   let bits = Int64.bits_of_float f in
             *   let lo = Int64.to_int (Int64.logand bits 0xFFFFFFFFL) in
             *   let hi = Int64.to_int (Int64.shift_right_logical bits 32) in
             * -- Bits_of_float.hi_lo_of_float64 gives the same two
             * 32-bit halves as (Int32.t * Int32.t); `land 0xffffffff`
             * recovers the same unsigned `int` interpretation
             * Int64.logand/to_int used to (Int32.to_int alone would
             * sign-extend a half with its top bit set into a negative
             * int). *)
            let (hi32, lo32) = Bits_of_float.hi_lo_of_float64 f in
            let hi = Int32.to_int hi32 land 0xffffffff in
            let lo = Int32.to_int lo32 land 0xffffffff in
            let (R rtmp) = rTMP in
            [ op_irr op_last (lo asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) (lo land 0xffff) rTMP rTMP;
              op_mfc_mtc true rtmp rt;
              op_irr op_last (hi asr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) (hi land 0xffff) rTMP rTMP;
              op_mfc_mtc true rtmp (rt + 1);
            ]
         ) }

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
    (* case 18, JAL variant: "JAL 0(R3)" -- a call through a function
     * pointer, real MIPS grammar (unlike ARM32/ARM64's own "BL
     * 0(Rn)" accommodation, see the grammar's own comment) --
     * confirmed against goken's real optab.c: `{ AJAL, C_NONE,
     * C_NONE, C_ZOREG, 18, 4, REGLINK }`, the same case-18 row as
     * JMP's indirect form just above, just with REGLINK (R31) as the
     * `o->param` fallback for the "rd" field instead of REGZERO
     * (`p->reg` is NREG for this simple "0(Rn)" form, so `r =
     * o->param` per goken's own `case 18` C code) -- a real JALR,
     * not JR, so the return address genuinely gets written to R31,
     * unlike JMP's own indirect form. `oprrr(AJAL) = OP(1,1)`, vs
     * `oprrr(AJMP) = OP(1,0)` just above. Found stress-testing real
     * lib_core/libc (fmt/dofmt.c's real "JAL 0(R3)"). *)
    | JAL { contents = (IndirectJump rt) } ->
        let op_jal = op 1 1 in
        { size = 8; x = None; binary = (fun () ->
           [ op_rrr op_jal rZERO rt rLINK; nop ]
         ) }

    (* case 39:	/* rfe ==> jmp+rfe */ *)
    (* claude: kernel-only "return from exception": JR (r) followed
     * by a fixed RFE instruction (goken's `oprrr(ARFE)` = MMU(2,0),
     * no operands) -- unlike every other jump/branch this session,
     * RFE itself fills JR's own mandatory delay slot by design (this
     * is the whole point: exception state is restored exactly as
     * the jump takes effect), so no extra nop is needed. Confirmed
     * via `vl -a` directly: exactly 2 words, no padding. Reuses the
     * same OP_RRR shape as case 18's JMP (r1=0, r2=target, r3=0). *)
    | RFE { contents = (IndirectJump rt) } ->
        { size = 8; x = None; binary = (fun () ->
            [ op_rrr (op 1 0) rZERO rt rZERO;
              sp 2 0 @ [ (16, 21); (2, 3); (0, 0) ];
            ]
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
         * output. See docs/claude_notes/mips_port.md.
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
     * docs/claude_notes/mips_port.md. *)
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
    (* case 6, BFPT/BFPF variant: no register operand at all (the
     * condition was already set by a preceding CMPxxF/CMPxxD), so
     * this reuses op_irr_no_r3 with rZERO for the unused bit-21
     * field, matching goken's own p->reg==NREG->0 default exactly.
     * See Ast_asmv.ml's own BFP comment. *)
    | BFP (is_true, _branch) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_irr_no_r3 (opirr_bfp_opcode is_true) (gbranch_offset node) rZERO; nop ]
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
        | Address (Global (global, goffset)) ->
              (* claude: no fast R30-relative path here -- see the
               * long comment on offset_to_R30/big above for why:
               * goken's own BIG=0 makes it permanently unreachable
               * in vl itself, so always fall through to loading the
               * full absolute address below. (offset_to_R30 is still
               * called from base_and_offset_of_entity for indirect
               * addressing -- e.g. O(R30) -- which is a different,
               * still-live code path; only the address-of-global
               * fast path here is dead.)
               *
               * claude: `goffset` (formerly `_offsetTODO`, silently
               * discarded) is a REAL, confirmed bug fix -- a genuine
               * "MOVW $sym+N(SB),Rt" with nonzero N never arises from
               * hand-written assembly (you'd just give each datum
               * its own zero-offset symbol), but real 7c-style
               * compilers pack every string literal in one C file
               * into a single shared ".string<>" blob, each addressed
               * at its own nonzero byte offset -- exactly ARM32's own
               * already-documented and already-fixed version of this
               * same bug (see arm_port.md's hello_libc section,
               * `_offsetTODO` in Codegen5.ml/Codegen.ml), just never
               * ported to MIPS's own Codegenv.ml until this stress
               * test found it too. Silently computed sym+0 before,
               * making e.g. fmt/dofmt.c's own shared digit-table
               * string read a few bytes into the WRONG string
               * literal -- confirmed exactly this way: hello.c's
               * "%d" formatting printed plausible-looking but wrong
               * ASCII (garbage read from unrelated string data)
               * instead of real digit characters. *)
              (* case 19:	/* mov $lcon,r ==> lu+or */ *)
              { size = 8; x = None; binary = (fun () ->
              (* similar to WORD case *)
              (* TODO: introduce helper lcon_address_of_global *)
              let v = Hashtbl.find env.syms (T.symbol_of_global global) in
              let lcon =
                match v with
                | T.SText2 real_pc -> real_pc + goffset
                | T.SData2 (offset, _kind) ->
                  (match init_data with
                  | None -> raise (Impossible "init_data should be set by now")
                  | Some init_data -> init_data + offset + goffset
                  )
                in
                [ op_irr op_last (lcon lsr 16) rZERO rt;
                  op_irr (opirr_arith_opcode OR) lcon rt rt;
                ]
              )}
        (* case 26:	/* mov $lsext/auto/oreg,,r2 ==> lu+or+add */ *)
        (* claude: address-of-local/param. Unlike Address-of-Global
         * just above, goken's C_SACON fast path (small SP-relative
         * offset, single ADDU) is ALSO permanently dead here for
         * the exact same reason as C_SECON/offset_to_R30: aclass()
         * gates it on `instoffset >= -BIG && instoffset < BIG`, and
         * BIG=0 on MIPS makes that condition unsatisfiable -- so
         * this always takes the generic LU+OR+ADDU path (case 26),
         * never the fast one, matching goken exactly. goken's C
         * also diags if p->to.reg is REGTMP itself; not replicated,
         * same reasoning as case 23's identical diag. *)
        | Address ((Local _ | Param _) as entity) ->
            { size = 12; x = None; binary = (fun () ->
                let (rbase, offset) =
                  base_and_offset_of_entity node env.syms env.autosize entity in
                [ op_irr op_last (offset asr 16) rZERO rTMP;
                  op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
                  op_rrr (oprrr_arith_opcode (ADD (W, U))) rTMP rbase rt;
                ]
             ) }
        )

    (* Store/Load *)

    (* claude: BIG=0 strikes again here, in a form that wasn't
     * previously flagged: span.c's aclass() for a plain D_NONE
     * register-indirect operand (Indirect(reg,off), no $ symbol)
     * returns C_ZOREG only for offset==0 exactly, else C_SOREG if
     * `instoffset >= -BIG && instoffset < BIG` -- which, same as
     * every other BIG-gated fast path this session, is
     * unsatisfiable for ANY offset since BIG=0. So C_SOREG is ALSO
     * permanently dead: case 7/8's single-instruction fast path
     * below is only ever reached for offset==0 (matching C_ZOREG,
     * which cmp() accepts wherever SOREG is needed); any OTHER
     * offset needs case 35/36's REGTMP-based expansion instead --
     * confirmed empirically against goken directly (`vl -a`:
     * `MOVW R1,4(R5)` takes the 4-instruction path, `MOVW R1,0(R5)`
     * the 1-instruction one). The old code here (a standing "TODO:
     * need look for offset if SOREG or LOREG" -- exactly this) took
     * the fast path unconditionally, previously untested since no
     * fixture used Indirect with a nonzero offset. Found while
     * porting case 27/28 (float memory access), which share this
     * exact SOREG/LOREG split.
     *
     * Separately: every LOAD below (case 8/36/27, not the STORE
     * cases 7/35/28) has its own mandatory MIPS I load-delay-slot
     * hazard -- the classic "the register loaded by lw isn't safe
     * to use in the very next instruction" -- confirmed via `vl -a`
     * directly for each variant (ZOREG/LOREG/Entity, int and
     * float). Same already-documented, out-of-scope scheduler gap
     * as every other one this session (goken's sched.c fills it
     * with a real hoisted instruction when eligible); this always
     * emits a plain nop. Previously-undetected gap in the existing
     * case 36 (committed earlier this session, before any fixture
     * exercised a bare register-indirect or Entity load) as well as
     * the new case 8/27 code -- no fixture had used Indirect
     * addressing at all until this investigation. *)

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
    (* case 35 (LOREG variant): same as just above, but the base is
     * already a plain register (no base_and_offset_of_entity
     * needed) -- see the BIG=0/SOREG comment above. *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rbase, offset))) when offset <> 0 ->
        { size = 16; x = None; binary = (fun () ->
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem W__ STR) 0 rTMP rf;
          ]
          ) }
    (* case 35 variant: "MOVW $0,off(Rbase)" -- storing a literal 0 to
     * a nonzero-offset register-indirect address. Confirmed byte-
     * identical against real goken by manually decoding a linked
     * reference binary's raw words: goken exploits R0 (hardware-
     * wired zero) as the store's own value register instead of
     * materializing a separate one, so this is the exact same
     * 3-instruction address computation as the plain register-source
     * case just above, plus a plain SW with rZERO as the value --
     * genuinely real goken behavior, not an xix-only expansion (only
     * scoped to the literal 0 case, since that's the only one any
     * real closure stress-tested so far has needed; a general
     * nonzero immediate would need its own REGTMP-materialize step,
     * not verified against real goken yet). Found stress-testing
     * real lib_core/libc (fmt/fltfmt.c's real "MOVW $0,8(R29)"). *)
    | Move2 (W__, Right (Int 0), Gen (Indirect (rbase, offset))) when offset <> 0 ->
        { size = 16; x = None; binary = (fun () ->
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem W__ STR) 0 rTMP rZERO;
          ]
          ) }
    (* case 35 variant, Entity/named-local sibling: "MOVW $0,w-40(SP)"
     * -- same rZERO-as-value trick as the plain Indirect variant just
     * above, just resolving the real address via
     * base_and_offset_of_entity first (matching the register-source
     * Entity case near the top of this case-35 family). Found
     * stress-testing real lib_core/libc (fmt/dofmt.c's real "MOVW
     * $0,w-40(SP)"). *)
    | Move2 (W__, Right (Int 0), Gen (Entity ent)) ->
        { size = 16; x = None; binary = (fun () ->
          let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
          in
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem W__ STR) 0 rTMP rZERO;
          ]
          ) }

    (* case 36:	/* mov lext/lauto/lreg,r ==> lw o(r30) */ *)
    | Move2 (W__, Left (Gen (Entity ent)), Gen (GReg rt)) ->
        { size = 20; x = None; binary = (fun () ->
            let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
            in
            let v = offset in
            [ op_irr op_last (v lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) v rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr (opirr_mem W__ LDR) 0 rTMP rt;
              nop;
            ]
          ) }
    (* case 36 (LOREG variant) -- see case 35's LOREG comment above. *)
    | Move2 (W__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) when offset <> 0 ->
        { size = 20; x = None; binary = (fun () ->
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr (opirr_mem W__ LDR) 0 rTMP rt;
              nop;
            ]
          ) }

    (* case 27:	/* mov [sl]ext/auto/oreg,fr ==> lwc1 o(r) */ *)
    (* claude: F__ (single-precision) only -- D__ needs two word
     * transfers (to freg+1 and freg, since a double occupies a
     * consecutive float-register pair) and opirr_mem doesn't have a
     * D__ encoding yet either (see its own "TODO: opirr_mem D__"),
     * left as a follow-up. Indirect splits into ZOREG (offset==0,
     * direct single instruction) vs LOREG (REGTMP-based, same as
     * case 35/36's LOREG variant) per the BIG=0/SOREG comment
     * above; Entity (SEXT/SAUTO/LEXT/LAUTO) always takes the slow
     * REGTMP path too, same as case 35/36's Entity variant. *)
    | Move2 (F__, Left (Gen (Indirect (R rbase, 0))), GFReg (FR rt)) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_irr_raw (opirr_mem F__ LDR) 0 rbase rt; nop ]
         ) }
    | Move2 (F__, Left (Gen (Indirect (R rbase, offset))), GFReg (FR rt)) ->
        { size = 20; x = None; binary = (fun () ->
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) (R rbase) rTMP rTMP;
              op_irr_raw (opirr_mem F__ LDR) 0 rtmp rt;
              nop;
            ]
         ) }
    | Move2 (F__, Left (Gen (Entity ent)), GFReg (FR rt)) ->
        { size = 20; x = None; binary = (fun () ->
            let (rbase, offset) =
                   base_and_offset_of_entity node env.syms env.autosize ent
            in
            let v = offset in
            let (R rtmp) = rTMP in
            [ op_irr op_last (v lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) v rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ LDR) 0 rtmp rt;
              nop;
            ]
          ) }

    (* case 28:	/* mov fr,[sl]ext/auto/oreg ==> swc1 o(r) */ *)
    (* claude: F__ only, same reasoning as case 27 above. goken's C
     * also diags if the base register is REGTMP itself (a real
     * assembler-error case for the slow-path arms, since REGTMP is
     * the scratch register for the address computation); not
     * replicated, same reasoning as case 23/26's identical diag. *)
    | Move2 (F__, Left (GFReg (FR fsrc)), Gen (Indirect (R rbase, 0))) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_irr_raw (opirr_mem F__ STR) 0 rbase fsrc ]
         ) }
    | Move2 (F__, Left (GFReg (FR fsrc)), Gen (Indirect (R rbase, offset))) ->
        { size = 16; x = None; binary = (fun () ->
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) (R rbase) rTMP rTMP;
              op_irr_raw (opirr_mem F__ STR) 0 rtmp fsrc;
            ]
         ) }
    | Move2 (F__, Left (GFReg (FR fsrc)), Gen (Entity ent)) ->
        { size = 16; x = None; binary = (fun () ->
            let (rbase, offset) =
                   base_and_offset_of_entity node env.syms env.autosize ent
            in
            let v = offset in
            let (R rtmp) = rTMP in
            [ op_irr op_last (v lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) v rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ STR) 0 rtmp fsrc;
            ]
          ) }

    (* case 27/28, D__ variant: MIPS32 has no native 64-bit FPU load/
     * store at all -- goken's own real case 27/28 C code splits a
     * double access into two F__-sized (LWC1/SWC1) halves against a
     * register PAIR (Fn/Fn+1), confirmed directly: the LOW address
     * word loads into Fn+1, the address+4 (high) word into Fn. This
     * port always takes the REGTMP-materialize-the-full-address path
     * (goken's own real "size 20" sub-case) regardless of whether a
     * smaller offset could fit a cheaper encoding (its own "size
     * 8/16" sub-cases) -- an xix-only simplification, not byte-
     * identical to goken's own tiered fast paths, but always
     * correct. Found stress-testing real lib_core/libc (fmt/
     * strtod.c's real "MOVD d+4(FP),F4"). *)
    | Move2 (D__, Left (Gen (Entity ent)), GFReg (FR rt)) ->
        { size = 24; x = None; binary = (fun () ->
            let (rbase, offset) =
                   base_and_offset_of_entity node env.syms env.autosize ent
            in
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ LDR) 0 rtmp (rt + 1);
              op_irr_raw (opirr_mem F__ LDR) 4 rtmp rt;
              nop;
            ]
          ) }
    | Move2 (D__, Left (GFReg (FR fsrc)), Gen (Entity ent)) ->
        { size = 20; x = None; binary = (fun () ->
            let (rbase, offset) =
                   base_and_offset_of_entity node env.syms env.autosize ent
            in
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ STR) 0 rtmp (fsrc + 1);
              op_irr_raw (opirr_mem F__ STR) 4 rtmp fsrc;
            ]
          ) }
    (* case 27/28, D__/Indirect variant: same as the Entity forms
     * just above, but the base is already a plain register (no
     * base_and_offset_of_entity needed) -- see the BIG=0/SOREG
     * comment elsewhere in this file for why even offset==0 still
     * needs the full REGTMP address computation here (unlike the
     * plain-word/F__ cases, this port doesn't bother with a separate
     * ZOREG fast path for D__, since every real closure need so far
     * has had a nonzero offset anyway). Found stress-testing real
     * lib_core/libc (fmt/dofmt.c's real "MOVD F0,8(R29)"). *)
    | Move2 (D__, Left (Gen (Indirect (rbase, offset))), GFReg (FR rt)) ->
        { size = 24; x = None; binary = (fun () ->
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ LDR) 0 rtmp (rt + 1);
              op_irr_raw (opirr_mem F__ LDR) 4 rtmp rt;
              nop;
            ]
          ) }
    | Move2 (D__, Left (GFReg (FR fsrc)), Gen (Indirect (rbase, offset))) ->
        { size = 20; x = None; binary = (fun () ->
            let (R rtmp) = rTMP in
            [ op_irr op_last (offset lsr 16) rZERO rTMP;
              op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
              op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
              op_irr_raw (opirr_mem F__ STR) 0 rtmp (fsrc + 1);
              op_irr_raw (opirr_mem F__ STR) 4 rtmp fsrc;
            ]
          ) }

    (* case 7:		/* mov r, soreg ==> sw o(r) */ *)
    (* claude: ZOREG (offset==0) only -- see the BIG=0/SOREG comment
     * on case 35/36 above for any other offset. *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rt, 0))) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_irr (opirr_mem W__ STR) 0 rt rf ]
         ) }
    (* case 7 variant: "MOVW $0,0(Rbase)" -- same rZERO-as-value trick
     * as case 35's own $0 variant above, just for the ZOREG (offset
     * == 0) fast path directly, no address materialization needed
     * at all. Found stress-testing real lib_core/libc. *)
    | Move2 (W__, Right (Int 0), Gen (Indirect (rt, 0))) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_irr (opirr_mem W__ STR) 0 rt rZERO ]
         ) }
    (* case 8:		/* mov soreg, r ==> lw o(r) */ *)
    | Move2 (W__, Left (Gen (Indirect (rf, 0))), Gen (GReg rt)) ->
         { size = 8; x = None; binary = (fun () ->
           [ op_irr (opirr_mem W__ LDR) 0 rf rt; nop ]
         ) }
    (* case 7/8, byte/halfword variant: MOVB/MOVBU/MOVH/MOVHU's own
     * memory forms (SB/SH store, LB/LBU/LH/LHU load) -- same ZOREG-
     * only scope as the plain word forms just above (BIG=0 makes any
     * nonzero offset need the case 35/36-style REGTMP expansion,
     * not yet ported for Move1). Found stress-testing real
     * lib_core/libc (utf/rune.c's real "MOVBU 0(R9),R3"). *)
    | Move1 (sz, Left (GReg rf), Indirect (rt, 0)) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_irr (opirr_mem1 sz STR) 0 rt rf ]
         ) }
    (* case 7 variant: "MOVBU $0,0(Rbase)" -- same rZERO-as-value
     * trick as the word-sized case 7/35 $0 variants above. Found
     * stress-testing real lib_core/libc. *)
    | Move1 (sz, Right (Int 0), Indirect (rt, 0)) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_irr (opirr_mem1 sz STR) 0 rt rZERO ]
         ) }
    (* case 35 variant, byte/halfword: "MOVB $0,off(Rbase)" -- same
     * REGTMP address-materialize + rZERO-as-value combo as the
     * word-sized case 35 $0 variant above, for a nonzero-offset
     * byte/halfword store. Found stress-testing real lib_core/libc
     * (utf/rune.c's real "MOVB $0,1(R8)"). *)
    | Move1 (sz, Right (Int 0), Indirect (rbase, offset)) when offset <> 0 ->
        { size = 16; x = None; binary = (fun () ->
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem1 sz STR) 0 rTMP rZERO;
          ]
          ) }
    | Move1 (sz, Left (Indirect (rf, 0)), GReg rt) ->
        { size = 8; x = None; binary = (fun () ->
          [ op_irr (opirr_mem1 sz LDR) 0 rf rt; nop ]
         ) }
    (* case 35/36, byte/halfword variant: same REGTMP address-
     * materialize expansion as the plain word Indirect forms above,
     * for a nonzero-offset register-indirect byte/halfword access
     * (BIG=0 rules out any single-instruction fast path here too).
     * Found stress-testing real lib_core/libc (fmt/dofmt.c's real
     * "MOVB 1(R9),R4"). *)
    | Move1 (sz, Left (GReg rf), Indirect (rbase, offset)) when offset <> 0 ->
        { size = 16; x = None; binary = (fun () ->
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem1 sz STR) 0 rTMP rf;
          ]
          ) }
    | Move1 (sz, Left (Indirect (rbase, offset)), GReg rt) when offset <> 0 ->
        { size = 20; x = None; binary = (fun () ->
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem1 sz LDR) 0 rTMP rt; nop;
          ]
          ) }
    (* case 35/36, byte/halfword variant: MOVB/MOVBU/MOVH/MOVHU
     * to/from a named local/param (real "x-4(SP)" addressing) --
     * same REGTMP address-materialize shape as case 35/36's own
     * word-sized Entity forms above, just with opirr_mem1 instead of
     * opirr_mem for the final access. Found stress-testing real
     * lib_core/libc (utf/rune.c's real "MOVB R4,x-4(SP)"). *)
    | Move1 (sz, Left (GReg rf), Entity ent) ->
        { size = 16; x = None; binary = (fun () ->
          let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
          in
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem1 sz STR) 0 rTMP rf;
          ]
          ) }
    | Move1 (sz, Left (Entity ent), GReg rt) ->
        { size = 20; x = None; binary = (fun () ->
          let (rbase, offset) =
                 base_and_offset_of_entity node env.syms env.autosize ent
          in
          [ op_irr op_last (offset lsr 16) rZERO rTMP;
            op_irr (opirr_arith_opcode OR) offset rTMP rTMP;
            op_rrr (oprrr_arith_opcode (ADD (W, U))) rbase rTMP rTMP;
            op_irr (opirr_mem1 sz LDR) 0 rTMP rt; nop;
          ]
          ) }

    (* case 47:	/* sc r, soreg */ *)
    (* claude: atomic store-conditional. Unlike case 7 (plain SW),
     * goken's optab.c only ever declares ASC/ALL with a SOREG memory
     * operand (C_REG,C_NONE,C_SOREG / C_SOREG,C_NONE,C_REG) -- no
     * $sym(SB)/FP/SP-relative form at all -- so there's no case
     * 35/36-style LOREG/Entity variant to port here; ZOREG
     * (offset==0) is the only shape that exists in goken itself, not
     * just the only one reachable (same BIG=0/SOREG story as case
     * 7/8, but here it's not even declared, not just dead). A store,
     * so no delay slot, same as case 7. *)
    | SC (rf, Indirect (rt, 0)) ->
        { size = 4; x = None; binary = (fun () ->
            [ op_irr (sp 7 0) 0 rt rf ]
         ) }

    (* case 48:	/* ll soreg, r */ *)
    (* claude: atomic load-linked -- ZOREG only, same reasoning as
     * case 47. A load, so it gets the same mandatory 1-NOP
     * load-delay-slot hazard as case 8/27/36 (confirmed via `vl
     * -a`). *)
    | LL (Indirect (rf, 0), rt) ->
        { size = 8; x = None; binary = (fun () ->
            [ op_irr (sp 6 0) 0 rf rt; nop ]
         ) }

    (* case 45/46:	/* case r */ / /* bcase $con,lbra */ -- NOT PORTED.
     * The jump-table switch-statement pair. Confirmed dead on this
     * arch: unreachable from .s source (no grammar rule in goken's
     * va/a.y), and goken's own MIPS compiler (vc/swt.c) never emits
     * it either -- its switch lowering is a binary-search of
     * branches instead, unlike 5c/7c/2c. No way to make goken itself
     * exercise this, so no way to test a port -- see
     * docs/claude_notes/mips_port.md.
     *)

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
     |TLB _|LL (_, _)|SC (_, _)
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




