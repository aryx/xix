(*s: Codegen5.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Eq.Operators
open Either

open Ast_asm
open Ast_asm5

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ARM code generation.
 *
 * The 'case <n>: ... ' comments below refer to code in 5l/asm.c so one
 * can easily check the corresponding C code in 5l that was used
 * as model for the OCaml code.
 * See also the '5l:' tag in comments to refer to the original C code.
 *
 * ocaml: No need for optab/oplook/ocmp/cmp as in 5l. Just use pattern matching!
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*s: type [[Codegen5.pool]] *)
type pool =
  (* note that it is not always an int! Sometimes it can be an
   * Address which will be resolved only at the very end.
   *)
  | PoolOperand of Ast_asm.ximm
  (* todo: still don't know why we need that *)
  | LPOOL 
(*e: type [[Codegen5.pool]] *)

(*s: type [[Codegen5.action]] *)
(*e: type [[Codegen5.action]] *)

(*s: type [[Codegen5.mem_opcode]] *)
type mem_opcode = LDR | STR
(*e: type [[Codegen5.mem_opcode]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Codegen5.error]] *)
let error (node : 'a T.node) (s : string) =
  failwith (spf "%s at %s on %s" s 
              (T.s_of_loc node.n_loc)
              (Types5.show_instr node.instr)
  )
(*e: function [[Codegen5.error]] *)

(*s: function [[Codegen5.int_of_bits]] *)
let int_of_bits (n : 'a T.node) (x : Bits.int32) : int =
  try
    Bits.int_of_bits32 x
  with Failure s -> error n s
(*e: function [[Codegen5.int_of_bits]] *)

(*s: function [[Codegen5.offset_to_R12]] *)
(* claude: BIG, ported from goken's 5l/l.h. R12 (aka SB, aka rSB
 * below) is set up at program start to point BIG bytes into the
 * data segment, not to its very start -- so a *later* MOVW $sym(SB)
 * can reach both "before" and "after" that point with a single
 * 12-bit-ish signed displacement, via `ADD $(offset-BIG), R12, Rt`,
 * instead of always going through the literal pool. See immrot
 * below for when that displacement is actually encodable: for a
 * small data segment (every fixture we have so far), offset-BIG
 * stays a large negative number that immrot can't encode, so this
 * fast path essentially never triggers yet -- but it needs to exist
 * and be correct for when it does (a big enough data segment, or a
 * symbol placed close enough to BIG).
 *)
let big = (1 lsl 12) - 4

let offset_to_R12 x = x - big
(*e: function [[Codegen5.offset_to_R12]] *)

(*s: function [[Codegen5.base_and_offset_of_indirect]] *)
let base_and_offset_of_indirect node symbols2 autosize x =
  match x with
  | Indirect (r, off) -> r, off 
  (* claude: this +4 used to be here, on Param, instead of on Local
   * below -- swapped after verifying against goken directly with
   * plain memory access (`MOVW x-8(FP), R1` / `MOVW x+8(SP), R1`,
   * no $, before touching any $lacon code). Nothing before
   * tests/linker/arm_diff/lacon_arm.s ever exercised Entity(Local)/
   * Entity(Param) at all (every earlier fixture used raw
   * Indirect(reg,off) syntax like `4(R13)` instead of a named
   * FP/SP-relative local), so the swap went uncaught. The reasoning
   * in the original comment (now on Local, below) still applies --
   * it was just attached to the wrong branch. *)
  (* note that for locals the offset is negative as in -4(SP), and so
   * will be converted as SP+autofize (to compute the old version of SP) - 4
   *)
  | Entity (Param (_s, off)) ->
      rSP, autosize + off
  (* remember that the +4 below is because we access the frame of the
   * caller which for sure is not a leaf. Note that autosize
   * here had possibly a +4 done if the current function
   * was also not a leaf, but still we need another +4 because what matters
   * now is the adjustment in the frame of the caller!
   *)
  | Entity (Local (_s, off)) ->
      rSP, autosize + 4 + off
  | Entity (Global (global, off)) ->
      let v = Hashtbl.find symbols2 (T.symbol_of_global global) in
      (match v with
        | T.SData2 (offset, _kind) ->
          rSB, offset_to_R12 (offset + off)
      (* stricter: allowed in 5l but I think with wrong codegen *)
      | T.SText2 _ -> 
          error node (spf "use of procedure %s in indirect with offset"
                       (A.s_of_global global))
      )
  | Imsr _ | Ximm _ | FImsr _ | FCRImsr _ | PSRImsr _ | RegList _ ->
      raise (Impossible "should be called only for indirects")
(*e: function [[Codegen5.base_and_offset_of_indirect]] *)

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

(*s: function [[Codegen5.immrot]] *)
(* claude: full port of goken's 5l/span.c immrot(ulong v). The C code
 * reads:
 *   for(i=0; i<16; i++) {
 *     if((v & ~0xff) == 0) return (1<<25)|(i<<8)|v;
 *     v = (v<<2) | (v>>30);
 *   }
 *   return 0;
 * which LOOKS like a 32-bit rotate-left-by-2 repeated up to 16 times
 * (checking, after each rotation, whether all set bits have been
 * brought into the low 8 bits) -- but `ulong` is 64 bits on this
 * host (`typedef unsigned long ulong` in include/core/types.h, and
 * `unsigned long` is 64-bit on x86-64/arm64 Linux). `v<<2` on a
 * 64-bit value does NOT wrap its top bits back around into `v>>30`
 * the way it would on a 32-bit host -- bits pushed past bit 63 are
 * simply lost, and `v>>30` after several iterations starts pulling
 * in bits that came from *lower* down in `v`, not from a genuine
 * 32-bit wraparound. So on this build immrot is, relative to true
 * ARM rotated-immediate semantics, subtly WRONG for some values that
 * are mathematically encodable but this specific (64-bit-`ulong`)
 * computation fails to recognize -- e.g. 0x9000 IS a valid ARM
 * rotated immediate (0x90 rotated), but this immrot() returns 0 for
 * it, so goken takes the literal-pool slow path (case 12) instead of
 * a single MOV/ADD (case 2/4) for that value on this build. Since
 * the whole point of this port is byte-for-byte matching THIS actual
 * goken binary, not "more correct" ARM codegen, this must replicate
 * the 64-bit non-wrapping computation exactly (caught by
 * tests/linker/arm_diff/swp_arm.s: `MOVW $0x9000, R4` diverged from
 * goken until this was fixed) -- hence Int64, not a 32-bit OCaml
 * int, to get real 64-bit unsigned truncating shifts.
 *
 * A negative x reinterprets as its 64-bit two's-complement pattern
 * directly (matching `long instoffset` -> `ulong` in C, both already
 * 64-bit on this host -- no 32-bit sign-extension step involved).
 * Returns None when no rotation works (goken returns a plain 0 in
 * that case; callers already treat `None` as "not encodable").
 *
 * TODO: return directly a Bits.t
 *)
let immrot x =
  let mask8 = 0xffL in
  let rec search i v =
    if i > 15 then None
    else if Int64.equal (Int64.logand v (Int64.lognot mask8)) 0L
    then Some (i, Int64.to_int (Int64.logand v mask8))
    else
      let v' = Int64.logor (Int64.shift_left v 2) (Int64.shift_right_logical v 30) in
      search (i + 1) v'
  in
  search 0 (Int64.of_int x)
(*e: function [[Codegen5.immrot]] *)

let rot_bit = (1, 25)

(*s: function [[Codegen5.immoffset]] *)
let immoffset x =
  (x >= 0 && x <= 0xfff) || (x < 0 && x >= -0xfff)
(*e: function [[Codegen5.immoffset]] *)

(* claude: case 50/51's short-offset range -- gfsr/gfsr_vfp's shared
 * 8-bit word-count field, i.e. a signed multiple-of-4 offset with
 * magnitude < 1024 (goken's ofsr/ovfpmem: `if(v&3) diag(...); else if
 * (v >= (1<<10)) diag(...)`). *)
let fimmoffset x =
  x mod 4 =|= 0 && ((x >= 0 && x < 0x400) || (x < 0 && x > -0x400))

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)
(* gxxx below means gen_binary_code of xxx *)

(*s: function [[Codegen5.gcond]] *)
(* 5l: was in part in opbra() *)
let gcond cond =
  match cond with
  | EQ     -> (0x0, 28)
  | NE     -> (0x1, 28)
  | GE (U) -> (0x2, 28)
  | LT (U) -> (0x3, 28)
  | MI     -> (0x4, 28)
  | PL     -> (0x5, 28)
  | VS     -> (0x6, 28)
  | VC     -> (0x7, 28)
  | GT (U) -> (0x8, 28)
  | LE (U) -> (0x9, 28) 
  | GE (S) -> (0xa, 28) 
  | LT (S) -> (0xb, 28)
  | GT (S) -> (0xc, 28)
  | LE (S) -> (0xd, 28)
  | AL     -> (0xe, 28)
  | NV     -> (0xf, 28)
(*e: function [[Codegen5.gcond]] *)


(*s: function [[Codegen5.gop_arith]] *)
(* 5l: was called oprrr() *)
let gop_arith op =
  match op with
  | AND -> (0x0, 21)
  | EOR -> (0x1, 21)
  | SUB -> (0x2, 21)
  | RSB -> (0x3, 21)
  | ADD -> (0x4, 21)
  | ADC -> (0x5, 21)
  | SBC -> (0x6, 21)
  | RSC -> (0x7, 21)
  (* TST 0x8, TEQ 0x9, CMP 0xa, CMN 0xb via gop_cmp below *)
  | ORR -> (0xc, 21)
  (* no reading syntax in 5a, but can be generated by 5l and used also
   * for part of SLL/SRL/SRA with gop_shift
   *)
  | MOV -> (0xd, 21) 
  | BIC -> (0xe, 21)
  | MVN -> (0xf, 21)

  | MUL | DIV | MOD -> raise (Impossible "should match those cases separately")
  | SLL | SRL | SRA -> raise (Impossible "should match those cases separately")
(*e: function [[Codegen5.gop_arith]] *)
  
(*s: function [[Codegen5.gsetbit]] *)
(* 5l: was part of oprrr() *)
let gsetbit opt =
  match opt with
  | None -> []
  | Some Set_condition -> [(1, 20)]
(*e: function [[Codegen5.gsetbit]] *)
  

(*s: function [[Codegen5.gop_shift]] *)
(* 5l: was part of oprrr. 
 * Must be called with gop_arith MOV in caller.
 *)
let gop_shift op =
  match op with
  | SLL -> (0, 5)
  | SRL -> (1, 5)
  | SRA -> (2, 5)
  | _ -> raise (Impossible "should match those cases separately")
(*e: function [[Codegen5.gop_shift]] *)


(*s: function [[Codegen5.gop_cmp]] *)
(* 5l: was part of oprrr before *)
let gop_cmp op =
  match op with
  (* Set_condition set by default for comparison opcodes *)
  | TST -> [(0x8, 21); (1, 20)]
  | TEQ -> [(0x9, 21); (1, 20)]
  | CMP -> [(0xa, 21); (1, 20)]
  | CMN -> [(0xb, 21); (1, 20)]
(*e: function [[Codegen5.gop_cmp]] *)

(* claude: goken's old ARM 7500 FP (coprocessor 1) encoding for the
 * dyadic arith ops -- ported from oprrr()'s AADDF/AADDD/etc cases.
 * Precision D sets an extra bit (7) on top of the same opcode used
 * for F. *)
let gop_arithf (op : arithf_opcode) (prec : A.floatp_precision) : Bits.t =
  let opcode = match op with
    | ADD_ -> 0x0 | MUL_ -> 0x1 | SUB_ -> 0x2 | DIV_ -> 0x4
  in
  [(0xe, 24); (opcode, 20); (1, 8)] @
  (match prec with A.F -> [] | A.D -> [(1, 7)])

(* claude: goken's oprrr() ACMPF/ACMPD case -- same bits regardless
 * of precision (no D-bit here, unlike gop_arithf above; that's what
 * the C code actually does, not an omission). *)
let gop_cmpf : Bits.t =
  [(0xe, 24); (0x9, 20); (0xf, 12); (1, 8); (1, 4)]

(* claude: VFP encoding for the dyadic arith ops -- ported from
 * opvfprrr()'s AADDF/AADDD/ASUBF/ASUBD/AMULF/AMULD/ADIVF/ADIVD
 * cases. Unlike gop_arithf's FPA encoding, the precision bit lives
 * inside the same 4-bit field as a fixed pattern (0xa for F, 0xb for
 * D at bits[11:8]), not a separate standalone bit; SUB additionally
 * sets bit6 (goken's `1<<6`) on top of the same bits[23:20]=0x3 ADD
 * uses. *)
let gop_arithf_vfp (op : arithf_opcode) (prec : A.floatp_precision) : Bits.t =
  let prec_nibble = match prec with A.F -> 0xa | A.D -> 0xb in
  (0xe, 24) ::
  (match op with
  | ADD_ -> [(prec_nibble, 8); (0x3, 20)]
  | SUB_ -> [(prec_nibble, 8); (0x3, 20); (1, 6)]
  | MUL_ -> [(prec_nibble, 8); (0x2, 20)]
  | DIV_ -> [(prec_nibble, 8); (0x8, 20)]
  )

(* claude: VFP encoding for CMPF/CMPD -- ported from opvfprrr()'s
 * ACMPF/ACMPD cases (same bits regardless of precision except the
 * bits[11:8] nibble, same as gop_arithf_vfp above). Unlike FPA's
 * gop_cmpf, this is only the *first* of two instructions goken
 * emits for case 75 -- see gop_cmpf_vfp_mrs below for the second. *)
let gop_cmpf_vfp (prec : A.floatp_precision) : Bits.t =
  let prec_nibble = match prec with A.F -> 0xa | A.D -> 0xb in
  [(0xe, 24); (prec_nibble, 8); (0xb, 20); (1, 6); (0x4, 16)]

(* claude: goken's case 75 always emits this fixed second instruction
 * after the VFP compare -- "MRS APSR_nzcv, FPSCR" -- to move VFP's
 * comparison-result flags into the ARM CPSR's NZCV bits, since
 * that's where a subsequent Bxx reads condition flags from. The
 * literal `0x0ef1fa10` is goken's own hardcoded encoding (only the
 * condition-code nibble varies). *)
let gop_cmpf_vfp_mrs (cond : condition) : Bits.t =
  [gcond cond; (0x0ef1fa10 land 0x0fffffff, 0)]

(* claude: goken's FREGTMP (include/objexec/5.out.h): the VFP scratch
 * float register (F15/D15) used to shuttle a raw bit pattern between
 * an ARM core register and a VFP register via VMOV, when converting
 * the "other" direction from the one being converted -- see case 76
 * below. Mirrors rTMP (R11) on the integer side. *)
let fREGTMP = 15

(* claude: goken's oprrr() AMOVWF/AMOVWD/AMOVFW/AMOVDW cases -- FPA's
 * (case 55) fix-and-float base encoding, shared shape for both
 * directions (only the dir bit at bit20 and the D-precision bit at
 * bit7 vary); register placement differs by direction and is added
 * by the caller (case 55 below), not here. *)
let gop_fixfloat (dir : [`ToFloat | `ToInt]) (prec : A.floatp_precision) : Bits.t =
  let dirbit = match dir with `ToFloat -> 0 | `ToInt -> 1 in
  [(0xe, 24); (dirbit, 20); (1, 8); (1, 4)] @
  (match prec with A.F -> [] | A.D -> [(1, 7)])

(* claude: VFP's case 76 (fix and float) is a genuinely different
 * shape from FPA's single-instruction case 55: converting between
 * an ARM core register and a VFP register needs an extra VMOV to
 * shuttle the raw bit pattern in/out of a VFP register first (VFP's
 * convert instructions only operate register-to-register within the
 * VFP register file, they can't read/write a core register
 * directly) -- ported from codegen.c's case 76 body + opvfprrr()'s
 * AMOVWF/AMOVWD/AMOVFW/AMOVDW encodings.
 * int -> float (MOVWF/MOVWD): [VMOV Sd=rt,Rm=rf ; convert Sd=rt,Sm=rt in place]
 * float -> int (MOVFW/MOVDW): [convert Sd=FREGTMP,Sm=rf ; VMOV Rd=rt,Sn=FREGTMP] *)
let gop_fixfloat_vfp_to_float (cond : condition) (prec : A.floatp_precision)
    (rt : int) (rf : int) : Bits.t list =
  let prec_nibble = match prec with A.F -> 0xa | A.D -> 0xb in
  [ (* VMOV F,R: literal 0x0e000a10 -- bits[23:20]=0 (the fixed "op"
     * bit that's 1 for the opposite direction below), rt/rf slots
     * (bits 19:16/15:12) both 0 in the base. *)
    [gcond cond; (0xe, 24); (rt, 16); (rf, 12); (0xa, 8); (0x1, 4)];
    (* AMOVWF/AMOVWD (opvfprrr), Sd=Sm=rt (convert in place). Unlike
     * the VMOV literal above, opvfprrr's own `0xe<<24` base isn't
     * implicit here -- it must be listed explicitly. *)
    [gcond cond; (0xe, 24); (0xb, 20); (0x8, 16); (rt, 12); (prec_nibble, 8);
     (0xc, 4); (rt, 0)];
  ]
let gop_fixfloat_vfp_to_int (cond : condition) (prec : A.floatp_precision)
    (rt : int) (rf : int) : Bits.t list =
  let prec_nibble = match prec with A.F -> 0xa | A.D -> 0xb in
  [ (* AMOVFW/AMOVDW (opvfprrr), Sd=FREGTMP, Sm=rf (convert into the
     * scratch reg) -- same `0xe<<24` note as above. *)
    [gcond cond; (0xe, 24); (0xb, 20); (0xd, 16); (fREGTMP, 12); (prec_nibble, 8);
     (0xc, 4); (rf, 0)];
    (* VMOV R,F: literal 0x0e100a10 -- bits[23:20]=1, the fixed "op"
     * bit distinguishing this direction from VMOV F,R above (a
     * previous draft mis-derived this as part of the bits[19:16]
     * register slot instead; it's a separate, non-overlapping
     * field). *)
    [gcond cond; (0xe, 24); (0x1, 20); (fREGTMP, 16); (rt, 12); (0xa, 8); (0x1, 4)];
  ]

(* claude: goken's float.c chipfloats[] -- the FPA coprocessor's
 * fixed set of 8 immediate constants (chipfloat() returns their
 * index, or -1 if not one of these -- which this port doesn't
 * replicate: an unencodable float immediate is a genuine assembler
 * error, `error node`, not a silent fallback like goken's own
 * `diag(); rf = 0` recovery). *)
let chipfloat (f : float) : int option =
  let chipfloats = [| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 0.5; 10.0 |] in
  let rec search i =
    if i >= Array.length chipfloats then None
    else if Float.equal chipfloats.(i) f then Some i
    else search (i + 1)
  in
  search 0

(*s: function [[Codegen5.gop_bitshift_register]] *)
let gop_bitshift_register op =
  match op with
  | Sh_logic_left   -> (0x0, 5)
  | Sh_logic_right  -> (0x1, 5)
  | Sh_arith_right  -> (0x2, 5)
  | Sh_rotate_right -> (0x3, 5)
(*e: function [[Codegen5.gop_bitshift_register]] *)

(*s: function [[Codegen5.gop_rcon]] *)
let gop_rcon x =
  match x with
  | Left (R r) -> [(r,8); (1, 4)]
  | Right i    -> [(i, 7); (0, 4)]
(*e: function [[Codegen5.gop_rcon]] *)

(*****************************************************************************)
(* More complex code generation helpers *)
(*****************************************************************************)

(*s: function [[Codegen5.gshift]] *)
(* 5l: was in ??? *)
let gshift (R rf) (op2 : shift_reg_op) rcon : Bits.t = 
  gop_rcon rcon @ [gop_bitshift_register op2; (rf, 0)]
(*e: function [[Codegen5.gshift]] *)

(*s: function [[Codegen5.gbranch_static]] *)
let gbranch_static (nsrc : 'a T.node) (cond : condition) (is_bl : bool) : Bits.t=
  match nsrc.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst -> 
      let dst_pc = ndst.real_pc in
      (* -8 as small ARM opti that assumes you always at least want to jmp
       * above the next instruction so JMP 8 is actually encoded in the ARM
       * as JMP 0. Not sure it's worth the additional complexity.
       *)
      let v = (dst_pc - nsrc.real_pc) - 8 in
      if v mod 4 <> 0
      then raise (Impossible "layout text wrong, not word aligned node");

      (* TODO: asr or lsr? *)
      let v = (v asr 2) land 0xffffff in
      (* less: stricter: warn if too big, but should never happens *)
      (* 5l: was in opbra() in 5l *)
      [gcond cond;
       (if is_bl then (0x1, 24) else (0x0, 24)); 
       (v, 0) 
       ]
(*e: function [[Codegen5.gbranch_static]] *)


(*s: function [[Codegen5.gmem]] *)
let gmem cond op move_size opt offset_or_rm (R rbase) (R rt) =
  [gcond cond; (0x1, 26) ] @
  (match opt with
  | None ->                  [(1, 24)] (* pre offset *)
  | Some PostOffsetWrite ->  [(0, 24)]
  | Some WriteAddressBase -> [(1, 24); (1, 21)]
  ) @
  [(match move_size with 
   | Word -> (0, 22) 
   | Byte _ -> (1, 22) 
   | HalfWord _ -> raise (Impossible "should use different pattern rule")
   );
   (match op with 
    | LDR -> (1, 20) 
    | STR -> (0, 20)
   );
   (rbase, 16); (rt, 12);
  ] @
  (match offset_or_rm with
  | Either.Left offset ->
      if offset >= 0
      then [(1, 23); (offset, 0)]
      else [(0, 23); (-offset, 0)]
  (* claude: register-offset addressing always adds (U bit set),
   * never subtracts -- matches goken's olr()/olrr(): olrr(a,sc,i,b,r)
   * calls olr(a,sc,i,b,r), passing the *register number* i as olr's
   * "offset" argument, and a register number is always >= 0, so
   * olr's `if(v>=0) o|=1<<23` unconditionally sets the U bit for
   * this addressing mode. This was a real bug (bit23 always 0,
   * silently doing SUB rN instead of ADD rN) -- caught by
   * tests/linker/arm_diff/longoff_arm.s, the first fixture to
   * actually exercise this path (case 30/31's REGTMP-offset
   * load/store). *)
  | Either.Right (R r) -> [(1, 25); (1, 23); (r, 0)]
  )
(*e: function [[Codegen5.gmem]] *)

(* claude: ARM's "Load/Store Halfword and Load Signed Byte/Halfword"
 * instruction class (STRH/LDRH/LDRSH/LDRSB) -- a distinct bit layout
 * from LDR/STR's gmem above. Immediate-offset form: the 8-bit
 * magnitude offset is split into two 4-bit nibbles (bits[11:8] and
 * bits[3:0]) with bit22=1 marking "immediate offset". Register-offset
 * form (case 72/73's long-offset REGTMP indexing): bit22=0, and the
 * offset register goes directly in bits[3:0] (no nibble split, no
 * magnitude limit) -- goken's oshrr/olhrr are olhr/oshr with bit22
 * XORed off. Either way bits[6:5] select the variant (01=unsigned
 * halfword, 11=signed halfword, 10=signed byte -- there is no
 * signed-byte *store*, byte stores don't care about sign, see case
 * 20/gmem above). Ported from goken's 5l/codegen.c's olhr/oshr/
 * olhrr/oshrr (STRH forms are the LDRH forms with the L bit toggled
 * off). *)
let ghalfword (op : mem_opcode) (kind : move_size) cond
    (offset_or_rm : (int, reg) Either.t) (R rbase) (R rt) : Bits.t =
  (* claude: goken's oshr/olhr for case 70/72 (STORE) never look at
   * p->as at all -- there's only one STRH, sign is meaningless when
   * storing, so it always uses the base SH=01 bits. Only case 71/73
   * (LOAD) XOR those bits based on p->as (AMOVB/AMOVH) to select
   * LDRSB/LDRSH/LDRH -- see olhr/oshr in codegen.c. *)
  let (bit6, bit5) =
    match op with
    | STR -> (0, 1)
    | LDR ->
        (match kind with
        | HalfWord U -> (0, 1)
        | HalfWord S -> (1, 1)
        | Byte S -> (1, 0)
        | Byte U | Word ->
            raise (Impossible "ghalfword only for HalfWord or signed Byte")
        )
  in
  let common = [
    gcond cond; (1, 24) (* P: pre-indexed, no writeback support here *);
    (match op with LDR -> (1, 20) | STR -> (0, 20));
    (rbase, 16); (rt, 12);
    (1, 7); (bit6, 6); (bit5, 5); (1, 4);
  ] in
  match offset_or_rm with
  | Either.Left offset ->
      let (u_bit, mag) = if offset >= 0 then (1, offset) else (0, -offset) in
      if mag >= 0x100
      then raise (Impossible "halfword/signed-byte offset too large (8-bit split immediate)");
      common @ [(u_bit, 23); (1, 22) (* I: immediate offset *);
                ((mag lsr 4) land 0xf, 8); (mag land 0xf, 0)]
  (* claude: register-offset mode always adds (U bit set), same
   * reasoning as gmem's Either.Right just above -- goken's
   * oshrr/olhrr pass the offset *register number* (always >= 0) as
   * olhr's "v" argument, whose `if(v>=0) o|=1<<23` always fires.
   * bit22 (I, "immediate offset") is 0 here: there's no immediate at
   * all, the low nibble is Rm directly, and bits[11:8] stay 0. *)
  | Either.Right (R r) ->
      common @ [(1, 23); (0, 22); (r, 0)]

(* claude: case 50/51 (FPA LDF/STF) -- ofsr() in codegen.c. Coprocessor
 * data-transfer class (bits 27-25=110), always pre-indexed (bit24=1
 * -- goken's own C ORs this bit in twice, once conditionally on
 * !P and once unconditionally right after, so it's set regardless;
 * not replicated as a real .P/post-index option since nothing here
 * ever produces one), coprocessor number 1 (bit8), offset is a
 * word-count (v>>2) in the low 8 bits (magnitude only -- caller
 * passes the sign via the U bit), and bit15 selects Double vs Float.
 * The L bit (load vs store) is added by the caller, not here, same
 * as goken's own case 51 (`ofsr(...) | (1<<20)`). *)
let gfsr (prec : A.floatp_precision) (offset : int) (R rbase) (R rf) : Bits.t =
  let double_bit = match prec with A.D -> [(1, 15)] | A.F -> [] in
  let (u_bit, mag) = if offset < 0 then (0, -offset) else (1, offset) in
  [(0x6, 25); (1, 24); (u_bit, 23); (1, 8);
   ((mag asr 2) land 0xff, 0); (rbase, 16); (rf, 12)] @ double_bit

(* claude: case 50/51's VFP encoding (VLDR/VSTR) -- ovfpmem() in
 * codegen.c. bits[27:24]=0xd marks the VLDR/VSTR class; the
 * coprocessor "size" nibble at bits[11:8] is 0xa/0xb (F/D), same
 * values gop_arithf_vfp already uses for the same distinction. The L
 * bit is added by the caller, same as gfsr above. *)
let gfsr_vfp (prec : A.floatp_precision) (offset : int) (R rbase) (R rf) : Bits.t =
  let prec_nibble = match prec with A.F -> 0xa | A.D -> 0xb in
  let (u_bit, mag) = if offset < 0 then (0, -offset) else (1, offset) in
  [(0xd, 24); (u_bit, 23); ((mag asr 2) land 0xff, 0);
   (rbase, 16); (rf, 12); (prec_nibble, 8)]

(* claude: case 56 (write to FP[CS]R) / case 57 (read from FP[CS]R).
 * Fixed coprocessor-1 register-transfer encoding (goken's codegen.c):
 * bits[27:24]=0xe, bit8/bit4 set, the FCR selector (FPSR=1, FPCR=2 --
 * goken's `(p->to.reg+1)`/`(p->from.reg+1)`, from lex.c's FPSR=0/
 * FPCR=1) at bits[22:21], and the int register at bits[15:12]. The L
 * bit (bit20, read vs write) is added by the caller, same convention
 * as gfsr/gfsr_vfp above. *)
let gfcr (fcr : fcrreg) (R rint) : Bits.t =
  let fcr_val = match fcr with FPSR -> 1 | FPCR -> 2 in
  [(0xe, 24); (1, 8); (1, 4); (fcr_val, 21); (rint, 12)]

let psr_bit (psr : psrreg) = match psr with CPSR -> 0 | SPSR -> 1

(* claude: case 35 (mov PSR,R -- MRS). goken's codegen.c: `o1 =
 * (2<<23)|(0xf<<16)|(0<<0); o1 |= (from.reg&1)<<22 | to.reg<<12`.
 * The `0xf<<16` field is a fixed "must be 1111" field in the real
 * MRS encoding (unrelated to any operand). *)
let gpsr_read (psr : psrreg) (R rt) : Bits.t =
  [(0x2, 23); (0xf, 16); (psr_bit psr, 22); (rt, 12)]

(* claude: case 36 (mov R,PSR) / case 37 (mov $con,PSR) -- MSR,
 * shared base encoding (goken's codegen.c: `o1 =
 * (2<<23)|(0x29f<<12)|(0<<4); ... o1 |= (to.reg&1)<<22`), then the
 * caller ORs in either a plain register (case 36, `from.reg<<0`) or
 * an immrot-encoded immediate (case 37, `immrot(instoffset)`) --
 * see psrreg's own comment for why the ".F" (flags-only) suffix
 * isn't wired (always the "full PSR write" encoding here). *)
let gpsr_write_base (psr : psrreg) : Bits.t =
  [(0x2, 23); (0x29f, 12); (psr_bit psr, 22)]

(* claude: case 38/39 (MOVM -> STM/LDM). goken's codegen.c: base
 * `o1 = (0x4<<25)`, then P/U/W bits at [24]/[23]/[21] (S at [22], not
 * wired -- see movm_addr_mode's Ast_asm5.ml comment) and finally the
 * L bit at [20] distinguishing LDM (1, case 39) from STM (0, case
 * 38) -- ARM's standard block-data-transfer encoding
 * (cond|100|P|U|S|W|L|Rn|register_list). *)
let gmovm_opcode (mode : movm_addr_mode) ~(is_load : bool) : Bits.t =
  [(0x4, 25)]
  @ (if mode.mm_pre then [(1, 24)] else [])
  @ (if mode.mm_up then [(1, 23)] else [])
  @ (if mode.mm_writeback then [(1, 21)] else [])
  @ (if is_load then [(1, 20)] else [])

(* claude: case 17 (MULL/MULLU/MULAL/MULALU, 64-bit long multiply) --
 * oprrr()'s 4 cases in codegen.c all share `(v<<21)|(0x9<<4)`, where
 * `v` packs sign (bit1: 1=signed) and accumulate (bit0: 1=yes) into
 * a fixed `4 lor ...` base (bit2 always set, marking the "long
 * multiply" opcode class): AMULLU=4(U,no-acc), AMULALU=5(U,acc),
 * AMULL=6(S,no-acc), AMULAL=7(S,acc) -- confirmed against goken
 * directly (a first version of this helper had the two bits
 * swapped, caught by mull_case17.s: "MULL R1,R2,(R3,R4)" produced
 * MULALU's bits (0xa nibble) instead of MULL's (0xc)). *)
let gmull_opcode (sign : A.sign) (accumulate : bool) : Bits.t =
  let v = 4 lor (match sign with A.S -> 2 | A.U -> 0) lor (if accumulate then 1 else 0) in
  [(v, 21); (0x9, 4)]

(*s: function [[Codegen5.gload_from_pool]] *)
let gload_from_pool (nsrc : 'a T.node) cond rt =
  match nsrc.branch with
  | None -> raise (Impossible "literal pool should be attached to node")
  | Some ndst ->
      (* less: could assert the dst node is a WORD *)
      let dst_pc = ndst.real_pc in
      let v = (dst_pc - nsrc.real_pc) - 8 in
      if v mod 4 <> 0
      then raise (Impossible "layout text wrong, not word aligned node");
      (* LDR v(R15), RT (usually R11) *)
      gmem cond LDR Word None (Left v) rPC rt
(*e: function [[Codegen5.gload_from_pool]] *)
      
(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(*s: function [[Codegen5.rules]] *)
(* conventions (matches the one used (inconsistently) in 5l):
 * - rf = register from (called Rm in refcard)
 * - rt = register to   (called Rd in refcard)
 * - r  = register middle (called Rn in refcard)
 *)
let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  match node.instr with
  (* Reusable *)
   | T.Virt _ | T.TEXT _ | T.WORD _ -> 
      Codegen.default_rules env init_data node

  | T.I (instr, cond) ->
    (match instr with
    (* case 54:	/* floating point arith */ *)
    (* case 74:	/* vfp floating point arith */ *)
    (* claude: goken picks the encoding at build time via a global
     * (`vfp = debug['f']` in 5l/span.c, off by default -- see
     * Flags.vfp); we do the same at codegen time via !Flags.vfp,
     * since our AST doesn't distinguish FPA-ArithF from
     * VFP-ArithF -- it's the same ADD_/SUB_/MUL_/DIV_ instructions
     * either way, just encoded differently. FPA (case 54, ARM 7500
     * coprocessor 1) is goken's default; case 54 also covers
     * CMPF/CMPD there (via `if(p->to.type==D_NONE) rt=0`), handled
     * by our separate CmpF arm just below since our AST already
     * splits that out. FPA immediate float operands are limited to
     * goken's chipfloats[] (float.c): exactly {0,1,2,3,4,5,0.5,10}
     * -- anything else is a genuine assembler error here
     * (`error node`), not goken's own silent `diag(); rf=0`
     * fallback. VFP (case 74) doesn't support float immediates at
     * all (goken diag()s on D_FCONST there), and our AST's
     * arithf_opcode has no MOVF/MOVD/MOVFD/MOVDF (the monadic
     * move/precision-conversion ops that also live in case 74 in
     * codegen.c), so this arm only ever needs the dyadic path. *)
    | ArithF ((op, prec), from, middle, (FR rt)) ->
        let r = match middle with Some (FR x) -> x | None -> rt in
        if !Flags.vfp
        then
          let rf = match from with
            | Either.Right (FR rf) -> rf
            | Either.Left _ ->
                error node "VFP arithmetic does not support float immediates"
          in
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_arithf_vfp op prec @ [(rt, 12); (r, 16); (rf, 0)] ]
          )}
        else
          let rf_bits = match from with
            | Either.Right (FR rf) -> [(rf, 0)]
            | Either.Left fval ->
                (match chipfloat fval with
                | Some idx -> [(idx, 0); (1, 3)]
                | None ->
                    error node (spf "float immediate %f not one of chipfloat's 8 constants" fval))
          in
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_arithf op prec @ [(r, 16); (rt, 12)] @ rf_bits ]
          )}

    (* case 54:	/* floating point arith */ -- CMPF/CMPD share this
     * case in codegen.c; kept as its own OCaml arm since Ast_asm5's
     * CmpF is already a separate constructor from ArithF. *)
    (* case 75:	/* vfp floating point compare */ *)
    (* claude: unlike FPA (single instruction, flags go straight to
     * CPSR via the coprocessor mechanism), VFP's compare needs a
     * *second* fixed instruction ("MRS APSR_nzcv, FPSCR",
     * gop_cmpf_vfp_mrs) to move its comparison result into the ARM
     * CPSR where a later Bxx reads condition flags from -- hence
     * size=8 here vs 4 for FPA. Also note the field layout differs
     * from FPA's gop_cmpf: VFP puts the 2nd (middle) operand at
     * bit12, not bit16. goken's case 75 also supports `CMPF $0.0,
     * Fx` (the only FCONST it allows), but Ast_asm5.CmpF has no
     * immediate variant at all (unlike ArithF), so that path isn't
     * reachable from this AST regardless. *)
    | CmpF (prec, (FR fa), (FR fb)) ->
        if !Flags.vfp
        then
          { size = 8; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_cmpf_vfp prec @ [(fb, 12); (fa, 0)];
              gop_cmpf_vfp_mrs cond;
            ]
          )}
        else
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_cmpf @ [(fb, 16); (fa, 0)] ]
          )}

    (* case 55:	/* floating point fix and float */ *)
    (* case 76:	/* vfp floating point fix and float */ *)
    (* claude: int -> float/double (MOVWF/MOVWD). See gop_fixfloat/
     * gop_fixfloat_vfp_to_float above for the two encodings; same
     * !Flags.vfp dispatch as ArithF/CmpF above. *)
    | MOVWF (prec, (R rf), (FR rt)) ->
        if !Flags.vfp
        then
          { size = 8; x = None; binary = (fun () ->
            gop_fixfloat_vfp_to_float cond prec rt rf
          )}
        else
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_fixfloat `ToFloat prec @ [(rt, 16); (rf, 12)] ]
          )}

    (* case 55:	/* floating point fix and float */ *)
    (* case 76:	/* vfp floating point fix and float */ *)
    (* claude: float/double -> int (MOVFW/MOVDW), the other
     * direction of the pair just above. *)
    | MOVFW (prec, (FR rf), (R rt)) ->
        if !Flags.vfp
        then
          { size = 8; x = None; binary = (fun () ->
            gop_fixfloat_vfp_to_int cond prec rt rf
          )}
        else
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @ gop_fixfloat `ToInt prec @ [(rf, 0); (rt, 12)] ]
          )}

    (* case 17: 64-bit long multiply, register-pair result -- see
     * gmull_opcode's comment above for the sign/accumulate bit
     * packing. Bit layout: r1 (from) at bits[11:8], r2 (middle) at
     * bits[3:0], hi at bits[19:16], lo at bits[15:12] -- goken's
     * `(rf<<8)|r|(rt<<16)|(rt2<<12)`. *)
    | MULL (sign, accumulate, (R r1), (R r2), (R hi), (R lo)) ->
        { size = 4; x = None; binary = (fun () ->
          [ [gcond cond] @ gmull_opcode sign accumulate
            @ [(r1, 8); (r2, 0); (hi, 16); (lo, 12)]
          ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Arithmetics *)
    (* --------------------------------------------------------------------- *)
    (* case 1:		/* op R,[R],R */ *)
    (* case 2:		/* movbu $I,[R],R */ *)
    (* case 3:		/* add R<<[IR],[R],R */ *)
    | Arith ((AND|ORR|EOR|ADD|SUB|BIC|ADC|SBC|RSB|RSC|MVN|MOV) as op, opt,
             from, middle, (R rt)) ->
        (* TODO: use typed register instead of int *)
        let r =
          if (op =*= MVN || op =*= MOV)
          then 0
          else
            (* TODO: use |||  *)
            match middle with
            | Some (R x) -> x
            | None -> rt
        in
        (match from with
        (* case 1:		/* op R,[R],R */ *)
        | Reg (R rf) ->
            { size = 4; x = None; binary = (fun () ->
              [[gcond cond; gop_arith op] @ gsetbit opt
                @ [(r, 16); (rt, 12); (rf, 0)]]
            )}
        (* case 3:		/* add R<<[IR],[R],R */ *)
        | Shift (a, b, c) ->
            { size = 4; x = None; binary = (fun () ->
              [[gcond cond; gop_arith op] @ gsetbit opt @ [(r, 16); (rt, 12)]
                @ gshift a b c]
            )}
        | Imm i ->
            (match immrot i with
            (* case 2:		/* movbu $I,[R],R */ *)
            | Some (rot, v) ->
                { size = 4; x = None; binary = (fun () ->
                  [[gcond cond; gop_arith op] @ gsetbit opt
                    @ [(r, 16); (rt, 12); rot_bit; (rot, 8); (v, 0)]]
                )}
            (* claude: case 13: /* op $lcon, [R], R */ -- immrot
             * failed, so load the constant into REGTMP via the
             * literal pool first (`omvl(p, &p->from, REGTMP)` in
             * codegen.c), then apply the real op using REGTMP as the
             * "from" register instead of the immediate. Two
             * instructions, hence size=8, unlike case 2's size=4. *)
            | None ->
                let (R rtmp) = rTMP in
                { size = 8; x = Some (PoolOperand (Ast_asm.Int i));
                  binary = (fun () ->
                    [ gload_from_pool node cond rTMP;
                      [gcond cond; gop_arith op] @ gsetbit opt
                        @ [(r, 16); (rt, 12); (rtmp, 0)]
                    ]
                )}
            )
        )

    (* case 8:		/* sll $c,[R],R -> mov (R<<$c),R */ *)
    (* case 9:		/* sll R,[R],R -> mov (R<<R),R */ *)
    | Arith ((SLL|SRL|SRA) as op, opt, from, middle, (R rt)) ->
        let r = 
          (* TODO: use ||| and typed register *)
          match middle with 
          | Some (R x) -> x 
          | None -> rt 
        in
        let from_part = 
          match from with
          (* case 8:		/* sll $c,[R],R -> mov (R<<$c),R */ *)
          | Imm i ->
              if i >= 0 && i <= 31
              then [(i, 7)]
              (* stricter: failwith, not silently truncate *)
              else error node (spf "shit value out of range %d" i)
          (* case 9:		/* sll R,[R],R -> mov (R<<R),R */ *)
          | Reg (R rf) -> [(rf, 8); (1, 4)]
          (* stricter: I added that *)
          | Shift _ -> error node "bitshift on shift operation not allowed"
        in
        { size = 4; x = None; binary = (fun () ->
          [[gcond cond; gop_arith MOV; gop_shift op] @ gsetbit opt @ [(rt, 12)]
            @ from_part @ [(r, 0)]]
        )}

    (* case 15:	/* mul r,[r,]r */ *)
    | Arith (MUL, opt, from, middle, (R rt)) ->
        let rf =
          match from with
          | Reg (R rf) -> rf
          (* stricter: better error message *)
          | Shift _ | Imm _ ->
              error node "MUL can take only register operands"
        in
        let r = 
          match middle with 
          | None -> rt 
          | Some (R x) -> x 
        in
        (* ?? *)
        let (r, rf) = if rt =|= r then (rf, rt) else (r, rf) in
        
        { size = 4; x = None; binary = (fun () ->
          [[gcond cond; (0x0, 21); (0x9, 4);] @ gsetbit opt 
            @ [(rt, 16); (rf, 8);  (r, 0) ]]
        )}

    (* case 1:		/* op R,[R],R */ *)
    (* case 2:		/* movbu $I,[R],R */ *)
    (* case 3:		/* add R<<[IR],[R],R */ *)
    (* TODO? in 5a what is the encoding for CMP instr? middle reg is
     * used?
     *)
    | Cmp (op, from, (R r)) ->
        let from_part = 
          match from with
          (* case 1:		/* op R,[R],R */ *)
          | Reg (R rf) -> [(rf, 0)]
          (* case 2:		/* movbu $I,[R],R */ *)
          | Imm i ->
              (match immrot i with
              | Some (rot, v) -> [rot_bit; (rot, 8); (v, 0)]
              | None -> error node "TODO"
              )
          (* case 3:		/* add R<<[IR],[R],R */ *)
          | Shift (a, b, c) -> gshift a b c
        in
        (* TODO: (0, 12) ?? *)
        { size = 4; x = None; binary = (fun () ->
          [[gcond cond] @ gop_cmp op @ [(r, 16); (0, 12)] @ from_part]
        )}

    (* claude: MOVW reuses the plain Arith cases: optab.c has AMOVW
     * rows at cases 1/2/3 too (e.g. `{ AMOVW, C_REG, C_NONE, C_REG,
     * 1, 4 }`), and codegen.c's case 1 body explicitly special-cases
     * AMOVW/AMVN to force r=0 -- same as gop_arith MOV's r=0 default
     * just below. *)
    | MOVE (Word, None, Imsr from, Imsr (Reg (R rt))) ->
        let r = if !Flags.kencc_compatible then rt else 0 in
        (match from with
        (* case 1:		/* op R,[R],R */ *)
        | Reg (R rf) ->
            { size = 4; x = None; binary = (fun () ->
              [[gcond cond; gop_arith MOV; (r, 16); (rt, 12); (rf, 0)]]
            )}
        (* case 3:		/* op R<<[IR],[R],R */ *)
        | Shift (a, b, c) ->
            { size = 4; x = None; binary = (fun () ->
              [[gcond cond; gop_arith MOV; (r, 16); (rt, 12)] @ gshift a b c]
            )}
        | Imm i ->
            (match immrot i with
            (* case 2:		/* op $I,[R],R */ *)
            | Some (rot, v) ->
                { size = 4; x = None; binary = (fun () ->
                  [[gcond cond; gop_arith MOV; (r, 16); (rt, 12);
                    rot_bit; (rot, 8); (v, 0)]]
                )}
            (* claude: case 12: /* movw $lcon, reg */ -- immrot failed
             * (doesn't fit a rotated-immediate), so fall back to the
             * same literal-pool mechanism as the address-of-global
             * slow path above; Ast_asm.Int is already handled
             * generically by Codegen.default_rules's WORD case when
             * the pool gets flushed. *)
            | None ->
                { size = 4; x = Some (PoolOperand (Ast_asm.Int i));
                  binary = (fun () -> [ gload_from_pool node cond (R rt) ]) }
            )
        )

    (* case 58:	/* movbu R,R -> AND $0xff, R, R */ *)
    | MOVE (Byte U, None, Imsr (Reg (R r)), Imsr (Reg (R rt))) ->
        { size = 4; x = None; binary = (fun () ->
          [[gcond cond; (1, 25); gop_arith AND; (r, 16); (rt, 12); (0xff, 0)]]
        )}

    (* case 14:	/* movb/movbu/movh/movhu R,R */ *)
    (* claude: MOVBU R,R is actually case 58 above, not this case --
     * goken's own case-14 optab rows are only AMOVB/AMOVH/AMOVHU
     * C_REG,C_REG. *)
    (* MOVB RF, RT  -> SLL 24, RF, RT; SRA 24, RT, RT -> MOV (RF << 24), RT;...
     * MOVH RF, RT  -> SLL 16, RF, RT; SRA 16, RT, RT -> ...
     * MOVHU RF, RT -> SLL 16, RF, RT; SRL 16, RT, RT ->
     *)
    | MOVE ((Byte _|HalfWord _)as size, None, Imsr(Reg(R rf)),Imsr(Reg(R rt)))->
        let rop =
          match size with
          | Byte U | HalfWord U -> SRL
          | Byte S   | HalfWord S -> SRA
          | Word -> raise (Impossible "size matched in pattern")
        in
        let sh =
          match size with
          | Byte _ -> 24
          | HalfWord _ -> 16
          | Word -> raise (Impossible "size matched in pattern")
        in
        { size = 8; x = None; binary = (fun () ->
          [
            [gcond cond; gop_arith MOV; (rt, 12); gop_shift SLL; (sh,7);(rf,0)];
            [gcond cond; gop_arith MOV; (rt, 12); gop_shift rop; (sh,7);(rt,0)];
          ]
        )}

    | Arith ((DIV|MOD), _, _, _, _) -> error node "TODO: DIV/MOD"

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* case 5:		/* bra s */ *)
    (* case 6:		/* b ,O(R) -> add $O,R,PC */ *)
    | B x ->
        if cond <> AL 
        then raise (Impossible "B should always be with AL");

        { size = 4; x = Some LPOOL; binary = (fun () ->
          match !x with
          (* case 5:		/* bra s */ *)
          | Absolute _ -> [ gbranch_static node AL false @ [(0x5, 25);] ]
          (* case 6:		/* b ,O(R) -> add $O,R,PC */ *)
          | IndirectJump (R r) ->
              let (R rt) = rPC in
              (* TODO? can have offset with IndirectJump ? *)
              let offset = [rot_bit; (0, 0)] in
              [ [gcond AL; gop_arith ADD; (r, 16); (rt, 12)] @ offset 
              ]
          | _ -> raise (Impossible "5a or 5l should have resolved this branch")
        )}
    (* case 5:		/* bra s */ *)
    (* case 7:		/* bl ,O(R) -> mov PC,link; add $O,R,PC */ *)
    | BL x ->
        (match !x with
        (* case 5:		/* bra s */ *)
        | Absolute _ -> 
            { size = 4; x = None; binary = (fun () ->
              [ gbranch_static node AL true @ [ (0x5, 25) ] ]
            )}
        (* case 7:		/* bl ,O(R) -> mov PC,link; add $O,R,PC */ *)
        (* BL (R) -> ADD $0, PC, LINK; ADD $0, R, PC *)
        | IndirectJump (R r) ->
           { size = 8; x = None; binary = (fun () ->
             let (R r2) = rPC in
             let (R rt) = rLINK in
             let zero = [ rot_bit; (0, 0) ] in
              (* TODO? can have offset with IndirectJump ? *)
             let offset = [ rot_bit; (0, 0) ] in
              [ 
                (* Remember that when PC is involved in input operand
                 * there is an implicit +8 which is perfect for our case.
                 *)
                [gcond cond; gop_arith ADD; (r2, 16); (rt, 12) ] @ zero;
                [gcond cond; gop_arith ADD; (r, 16);  (r2, 12) ] @ offset;
              ]
             )}
        | _ -> raise (Impossible "5a or 5l should have resolved this branch")
        )

    (* case 5:		/* bra s */ *)
    (* claude: conditional branches (ABEQ/ABNE/...) share the same
     * optab case as unconditional B/BL -- only p->scond differs,
     * e.g. `{ ABEQ, C_NONE, C_NONE, C_BRANCH, 5, 4 }` in optab.c *)
    | Bxx (cond2, x) ->
        if cond <> AL 
        then raise (Impossible "Bxx should always be with AL");
        (match !x with
        | Absolute _ -> 
            { size = 4; x = None; binary = (fun () ->
              [ gbranch_static node cond2 false @  [(0x5, 25) ] ]
            )}
        (* stricter: better error message at least? *)
        | IndirectJump _ -> error node "Bxx supports only static jumps"
        | _ -> raise (Impossible "5a or 5l should have resolved this branch")
        )

    (* --------------------------------------------------------------------- *)
    (* Memory *)
    (* --------------------------------------------------------------------- *)

    (* Address *)
    (* claude: case 4/12 split: fast path (small SB-relative offset)
     * is case 4 (`add $I,[R],R`, optab.c's `{ AMOVW, C_RECON,
     * C_NONE, C_REG, 4, 4, REGSB }`); slow path (offset too big,
     * needs a literal pool word) is case 12 (`movw $lcon,reg`,
     * optab.c's `{ AMOVW, C_LCON, C_NONE, C_REG, 12, 4, 0, LFROM }`
     * -- `omvl()`'s LFROM-flag literal-pool load), same split as the
     * `from_part_when_small_offset_to_R12` match below. *)
    | MOVE (Word, None, Ximm ximm, Imsr (Reg (R rt))) ->
        (match ximm with
        | Int _ | Float _ -> 
           failwith "TODO: ?? because of refactor of imm_or_ximm"
        | String _ -> 
            (* stricter? what does 5l do with that? confusing I think *)
            error node "string not allowed in MOVW; use DATA"
        | Address (Global (global, _offsetTODO)) ->
            let from_part_when_small_offset_to_R12 =
              try 
                let v = Hashtbl.find env.syms (T.symbol_of_global global) in
                match v with
                | T.SData2 (offset, _kind) ->
                    let final_offset = offset_to_R12 offset in
                    (* super important condition! for bootstrapping
                     * setR12 in MOVW $setR12(SB), R12 and not
                     * transform it in ADD offset_set_R12, R12, R12.
                     *)

                    if final_offset =|= 0 
                    then None
                    else immrot final_offset
                | T.SText2 _ -> None
              (* layout_text has not been fully done yet so we may have
               * the address of a procedure we don't know yet
               *)
              with Not_found -> None
            in
            (match from_part_when_small_offset_to_R12 with
            (* case 4:		/* add $I,[R],R */ *)
            | Some (rot, v) ->
              (* MOVW $x(SB), RT -> ADD $offset_to_r12, R12, RT  *)
              { size = 4; x = None; binary = (fun () ->
                let (R r) = rSB in
                [[gcond cond; (1, 25); gop_arith ADD; (r, 16); (rt, 12);
                  (rot, 8); (v, 0)]]
            )}
            (* case 12:	/* movw $lcon, reg */ *)
            | None ->
              (* MOVW $L(SB), RT -> LDR x(R15), RT *)
              { size = 4; x = Some (PoolOperand ximm); binary = (fun () ->
                [ gload_from_pool node cond (R rt) ]
              )}
            )
        (* claude: address of a local/auto (SP-relative) or param
         * (FP-relative, but our port has no separate FP -- see
         * base_and_offset_of_indirect, which already folds Param
         * into an SP+autosize+4 offset the same way the Indirect/
         * Entity memory-operand cases do). Mirrors the Global case
         * just above: fast path is case 4 (fits immrot -> single
         * ADD), slow path is case 34 (doesn't fit -> REGTMP via
         * literal pool, then ADD).
         *
         * RACON-vs-LACON classification quirk: goken's aclass()
         * (span.c) decides RACON/LACON using the offset *before*
         * Rewrite5.rewrite's own "+4 for RLINK-save" adjustment to
         * autosize (`n.instr <- T.TEXT (..., size+4)`), which by the
         * time Codegen5 runs has already been baked permanently into
         * env.autosize -- the original, pre-adjustment frame size is
         * gone. Confirmed empirically (frame=$8192, off=-8: the fully
         * adjusted offset is 8192, which fits immrot, but goken still
         * emits the slow/pool form, because ITS classification value
         * is 8192-8=8184, which does not fit). So the fits-check
         * below is done against a *reconstructed* pre-adjustment
         * offset (env.autosize - 4, undoing that +4), while the
         * actual encoded value in either branch still uses the real,
         * fully-adjusted `offset`. *)
        | Address ((Local _ | Param _) as entity) ->
            let (rbase, offset) =
              base_and_offset_of_indirect node env.syms env.autosize
                (Entity entity) in
            let (R r) = rbase in
            let classification_offset =
              let off = match entity with
                | Local (_, off) | Param (_, off) -> off
                | Global _ -> raise (Impossible "matched above")
              in
              env.autosize - 4 + off
            in
            (match immrot classification_offset with
            (* case 4:		/* add $I,[R],R */ *)
            | Some _ ->
                (match immrot offset with
                | Some (rot, v) ->
                    { size = 4; x = None; binary = (fun () ->
                      [[gcond cond; (1, 25); gop_arith ADD; (r, 16); (rt, 12);
                        (rot, 8); (v, 0)]]
                    )}
                | None ->
                    raise (Impossible
                      "classification offset fits immrot but final offset doesn't"))
            (* case 34:	/* mov $lacon,R -> LDR x(R15), R11; ADD R11, R13, R */ *)
            | None ->
                let (R rtmp) = rTMP in
                { size = 8; x = Some (PoolOperand (Ast_asm.Int offset));
                  binary = (fun () ->
                    [ gload_from_pool node cond rTMP;
                      [gcond cond; gop_arith ADD; (r, 16); (rt, 12); (rtmp, 0)]
                    ]
                )}
            )
        )

    (* Load *)

    (* case 21:	/* mov/movbu O(R),R */ *)
    (* claude: short (12-bit) SB/SP/plain-register-relative offset;
     * optab.c's rows are AMOVW/AMOVBU only (C_SEXT/C_SAUTO/C_SOREG)
     * -- signed byte/halfword loads need extra shift-based sign
     * extension and go through case 22 instead, hence the `Byte U`
     * (not `Byte _`) restriction here matching optab.c exactly. *)
    | MOVE ((Word | Byte U) as size, opt, from, Imsr (Reg rt)) ->
        (match from with
        | Imsr (Imm _ | Reg _) -> 
            if size =*= Word 
            then raise (Impossible "pattern covered before")
            else error node "illegal combination?"
        | Imsr (Shift _) -> error node "TODO"
        | Ximm _ -> 
            if size =*= Word 
            then raise (Impossible "pattern covered before")
            else error node "illegal combination"
        | FImsr _ -> raise (Impossible "FImsr is MOVEF-only")
        (* case 57:	/* mov PSR,R */ -- FP[CS]R read (see the
         * FCRImsr write side, case 56, just above/in the store
         * arm). *)
        | FCRImsr fcr ->
            (match size with
            | Word ->
                { size = 4; x = None; binary = (fun () ->
                  [ [gcond cond] @ gfcr fcr rt @ [(1, 20)] ]
                )}
            | Byte _ | HalfWord _ -> error node "MOV from FP[CS]R must be Word")
        (* case 35:	/* mov PSR,R */ *)
        | PSRImsr psr ->
            (match size with
            | Word ->
                { size = 4; x = None; binary = (fun () ->
                  [ [gcond cond] @ gpsr_read psr rt ]
                )}
            | Byte _ | HalfWord _ -> error node "MOV from PSR must be Word")
        | RegList _ -> raise (Impossible "RegList is MOVM-only")
        | Indirect _ | Entity _ ->
            let (rbase, offset) =
              base_and_offset_of_indirect node env.syms env.autosize from in
            if immoffset offset
            then
              { size = 4; x = None; binary = (fun () ->
                [ gmem cond LDR size opt (Left offset) rbase rt ]
              )}
            else
              (* case 31:	/* mov/movbu L(R),R */ *)
              (* claude: offset too big for LDR's 12-bit immediate:
               * load it into REGTMP via the literal pool first
               * (`omvl(p, &p->from, REGTMP)` in codegen.c), then do
               * a register-offset LDR (Rbase + REGTMP) -- reusing
               * gmem's existing `Either.Right` (register-offset)
               * path, which case 21 above never needed. *)
              { size = 8; x = Some (PoolOperand (Ast_asm.Int offset));
                binary = (fun () ->
                  [ gload_from_pool node cond rTMP;
                    gmem cond LDR size opt (Right rTMP) rbase rt
                  ]
              )}
        )

    (* case 22:	/* movb/movh/movhu O(R),R -> lr,shl,shr */ *)
    (* claude: NOT ported, see case 71 just below (real ARMv4T
     * LDRSB/LDRSH/LDRH): goken's buildop() only keeps this
     * byte-split-and-shift fallback when `armv4` is unset
     * (`armv4 = !debug['h']`, 5l/span.c), i.e. only when goken is
     * invoked with -h -- which this harness never does, so case
     * 70-73's real instructions always win instead. Porting this
     * would be porting dead code no test could verify. *)
    (* case 71:	/* movb/movh/movhu O(R),R -> ldrsb/ldrsh/ldrh */ *)
    (* claude: signed byte / any halfword short-offset load, using the
     * real ARMv4T LDRSB/LDRSH/LDRH instructions (see ghalfword above).
     * Together with case 21 (Word/Byte U) this covers every
     * move_size. *)
    | MOVE ((Byte S | HalfWord _) as size, _opt, from, Imsr (Reg (R rt))) ->
        (match from with
        | Imsr _ | Ximm _ -> error node "illegal combination?"
        | FImsr _ -> raise (Impossible "FImsr is MOVEF-only")
        | FCRImsr _ -> raise (Impossible "FCRImsr is Word-MOVE-only")
        | PSRImsr _ -> raise (Impossible "PSRImsr is Word-MOVE-only")
        | RegList _ -> raise (Impossible "RegList is MOVM-only")
        | Indirect _ | Entity _ ->
            let (rbase, offset) =
              base_and_offset_of_indirect node env.syms env.autosize from in
            if abs offset < 0x100
            then
              { size = 4; x = None; binary = (fun () ->
                [ ghalfword LDR size cond (Left offset) rbase (R rt) ]
              )}
            else
              (* case 73:	/* movb/movh/movhu L(R),R -> ldrsb/ldrsh/ldrh */ *)
              (* claude: same REGTMP-via-pool pattern as case 31
               * above, using ghalfword's register-offset form
               * instead of gmem's. *)
              { size = 8; x = Some (PoolOperand (Ast_asm.Int offset));
                binary = (fun () ->
                  [ gload_from_pool node cond rTMP;
                    ghalfword LDR size cond (Right rTMP) rbase (R rt)
                  ]
              )}
        )

    (* case 32:	/* movh/movb L(R),R */ *)
    (* claude: NOT ported, same reasoning as case 22 above (dead code
     * under goken's default `armv4`; see case 73 just above for the
     * real instruction). *)

    (* Store *)

    (* case 20:	/* mov/movb/movbu R,O(R) */ *)
    (* claude: unlike the load side (case 21, Byte U only), STRB
     * doesn't care about signedness -- optab.c has both AMOVB and
     * AMOVBU rows for this case, hence `Byte _` (not just `Byte U`)
     * here.
     * note that works for Byte Signed and Unsigned here *)
    | MOVE ((Word | Byte _) as size, opt, Imsr (Reg rf), dest) ->
        (match dest with
        | Imsr (Reg _) -> raise (Impossible "pattern covered before")
        (* stricter: better error message *)
        | Imsr _ | Ximm _ -> 
            error node "illegal to store in an (extended) immediate"
        | FImsr _ -> raise (Impossible "FImsr is MOVEF-only")
        (* case 56:	/* move to FP[CS]R */ *)
        | FCRImsr fcr ->
            (match size with
            | Word ->
                { size = 4; x = None; binary = (fun () ->
                  [ [gcond cond] @ gfcr fcr rf ]
                )}
            | Byte _ | HalfWord _ -> error node "MOV to FP[CS]R must be Word")
        (* case 36:	/* mov R,PSR */ *)
        | PSRImsr psr ->
            (match size with
            | Word ->
                let (R rf_i) = rf in
                { size = 4; x = None; binary = (fun () ->
                  [ [gcond cond] @ gpsr_write_base psr @ [(rf_i, 0)] ]
                )}
            | Byte _ | HalfWord _ -> error node "MOV to PSR must be Word")
        | RegList _ -> raise (Impossible "RegList is MOVM-only")
        | Indirect _ | Entity _ ->
            let (rbase, offset) =
              base_and_offset_of_indirect node env.syms env.autosize dest in
            if immoffset offset
            then
              { size = 4; x = None; binary = (fun () ->
                [ gmem cond STR size opt (Left offset) rbase rf ]
              )}
            else
              (* case 30:	/* mov/movb/movbu R,L(R) */ *)
              (* claude: offset too big for STR's 12-bit immediate,
               * same reasoning as case 31 above: load it into REGTMP
               * via the literal pool, then a register-offset STR. *)
              { size = 8; x = Some (PoolOperand (Ast_asm.Int offset));
                binary = (fun () ->
                  [ gload_from_pool node cond rTMP;
                    gmem cond STR size opt (Right rTMP) rbase rf
                  ]
              )}
        )

    (* case 37:	/* mov $con,PSR */ -- a new top-level arm (the
     * source here is an immediate, not a register, unlike case 36
     * just above -- goken's own `aclass(&p->from)` + immrot dance,
     * no literal-pool fallback: goken diag()s if the constant isn't
     * immrot-encodable, so this errors loudly the same way rather
     * than silently emitting wrong bytes. *)
    | MOVE (Word, _, Imsr (Imm i), PSRImsr psr) ->
        (match immrot i with
        | Some (rot, v) ->
            { size = 4; x = None; binary = (fun () ->
              [ [gcond cond] @ gpsr_write_base psr @ [rot_bit; (rot, 8); (v, 0)] ]
            )}
        | None -> error node "immediate not encodable for MOV $con,PSR")

    (* case 23:	/* movh/movhu R,O(R) -> sb,sb */ *)
    (* claude: NOT ported, same reasoning as case 22 above (dead code
     * under goken's default `armv4`; see case 70 just below for the
     * real instruction). *)
    (* case 33:	/* movh/movhu R,L(R) -> sb, sb */ *)
    (* claude: NOT ported, same reasoning as case 23 just above (dead
     * code under goken's default `armv4`; see case 72 just below for
     * the real instruction). *)
    (* case 70:	/* movh/movhu R,O(R) -> strh */ *)
    (* claude: halfword short-offset store, using the real ARMv4T STRH
     * instruction (see ghalfword above). *)
    | MOVE ((HalfWord _) as size, _opt, Imsr (Reg rf), dest) ->
        (match dest with
        | Imsr _ | Ximm _ ->
            error node "illegal to store in an (extended) immediate"
        | FImsr _ -> raise (Impossible "FImsr is MOVEF-only")
        | FCRImsr _ -> raise (Impossible "FCRImsr is Word-MOVE-only")
        | PSRImsr _ -> raise (Impossible "PSRImsr is Word-MOVE-only")
        | RegList _ -> raise (Impossible "RegList is MOVM-only")
        | Indirect _ | Entity _ ->
            let (rbase, offset) =
              base_and_offset_of_indirect node env.syms env.autosize dest in
            if abs offset < 0x100
            then
              { size = 4; x = None; binary = (fun () ->
                [ ghalfword STR size cond (Left offset) rbase rf ]
              )}
            else
              (* case 72:	/* movh/movhu R,L(R) -> strh */ *)
              (* claude: same REGTMP-via-pool pattern as case 30
               * above, using ghalfword's register-offset form. *)
              { size = 8; x = Some (PoolOperand (Ast_asm.Int offset));
                binary = (fun () ->
                  [ gload_from_pool node cond rTMP;
                    ghalfword STR size cond (Right rTMP) rbase rf
                  ]
              )}
        )

    (* case 59/60/61: movw/bu R<<I(R),R -> ldr indexed / movb R(R),R
     * -> ldrsb indexed / movw/b/bu R,R<<[IR](R) -> str indexed --
     * NOT PORTED. Same shape as case 62/63 (see arm_port.md):
     * goken's 5a grammar has no rule combining a shift operand with a
     * base register at all, so this addressing mode can't be spelled
     * in real .s text either -- compilers/5c/peep.c confirms it's a
     * compiler peephole (fusing a separate shift + load/store into
     * one D_SHIFT-operand instruction), not something 5a ever parses.
     * No way to build a byte-identical fixture against 5a/5l for it;
     * left unimplemented rather than repeating the CASE/BCASE
     * detour (see that entry's writeup for the fuller cost/benefit).
     * Also unlikely to ever be needed: xix's own future C compiler
     * (occ) is planned to use c-- as a backend, which does its own
     * peepholing and emits plain, unfused assembly -- not this exact
     * fused-shift-into-addressing-mode shape goken's 5c produces.
     *)

    (* case 50/51: floating point store/load -- MOVF/MOVD to/from a
     * short-offset SB/SP/plain-register-relative address, the float
     * analogue of case 20/21 (int store/load). Same
     * base_and_offset_of_indirect reuse (the memory side always
     * addresses through a plain *integer* base register regardless
     * of the data register's type), just gated by fimmoffset's
     * narrower 10-bit-word-count range instead of immoffset's 12-bit
     * one, and dispatched through gfsr/gfsr_vfp per !Flags.vfp
     * (same convention as ArithF/CmpF/MOVWF above). *)
    | MOVEF (prec, FImsr (FR rf), ((Indirect _ | Entity _) as dest)) ->
        let (rbase, offset) =
          base_and_offset_of_indirect node env.syms env.autosize dest in
        if fimmoffset offset
        then
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @
              (if !Flags.vfp then gfsr_vfp prec offset rbase (R rf)
               else gfsr prec offset rbase (R rf))
            ]
          )}
        else
          (* case 52: floating point store, long offset UGLY -- no
           * register-offset addressing mode exists for FPA/VFP
           * load/store (unlike gmem's int STR/LDR), so the full
           * address must be computed explicitly into REGTMP first
           * (`ADD REGTMP,Rbase,REGTMP` after loading the offset from
           * the pool), then a zero-offset store relative to REGTMP.
           *)
          { size = 12; x = Some (PoolOperand (Ast_asm.Int offset));
            binary = (fun () ->
              let (R rbase_i) = rbase and (R rtmp_i) = rTMP in
              [ gload_from_pool node cond rTMP;
                [gcond cond; gop_arith ADD; (rtmp_i, 16); (rtmp_i, 12); (rbase_i, 0)];
                [gcond cond] @
                (if !Flags.vfp then gfsr_vfp prec 0 rTMP (R rf)
                 else gfsr prec 0 rTMP (R rf))
              ]
          )}

    | MOVEF (prec, ((Indirect _ | Entity _) as src), FImsr (FR rt)) ->
        let (rbase, offset) =
          base_and_offset_of_indirect node env.syms env.autosize src in
        if fimmoffset offset
        then
          { size = 4; x = None; binary = (fun () ->
            [ [gcond cond] @
              (if !Flags.vfp then gfsr_vfp prec offset rbase (R rt)
               else gfsr prec offset rbase (R rt)) @ [(1, 20)]
            ]
          )}
        else
          (* case 53: floating point load, long offset UGLY -- same
           * REGTMP-address-then-zero-offset trick as case 52. *)
          { size = 12; x = Some (PoolOperand (Ast_asm.Int offset));
            binary = (fun () ->
              let (R rbase_i) = rbase and (R rtmp_i) = rTMP in
              [ gload_from_pool node cond rTMP;
                [gcond cond; gop_arith ADD; (rtmp_i, 16); (rtmp_i, 12); (rbase_i, 0)];
                [gcond cond] @
                (if !Flags.vfp then gfsr_vfp prec 0 rTMP (R rt)
                 else gfsr prec 0 rTMP (R rt)) @ [(1, 20)]
              ]
          )}

    | MOVEF (_, _, _) ->
        error node "illegal MOVEF operand combination"

    (* Multiple move (block data transfer) *)
    (* case 38:	/* movm $con,oreg -> stm */ -- store: registers to
     * memory. Offset must be zero (goken's own `aclass` + "offset
     * must be zero in MOVM" diag()), so this errors loudly rather
     * than silently ignoring a nonzero offset, same precedent as
     * case 37's immrot check. *)
    | MOVM (mode, RegList bits, Indirect (rbase, offset)) ->
        if offset <> 0 then error node "offset must be zero in MOVM";
        let (R rbase_i) = rbase in
        { size = 4; x = None; binary = (fun () ->
          [ [gcond cond] @ gmovm_opcode mode ~is_load:false
            @ [(rbase_i, 16); (bits, 0)] ]
        )}
    (* case 39:	/* movm oreg,$con -> ldm */ -- load: memory to
     * registers. *)
    | MOVM (mode, Indirect (rbase, offset), RegList bits) ->
        if offset <> 0 then error node "offset must be zero in MOVM";
        let (R rbase_i) = rbase in
        { size = 4; x = None; binary = (fun () ->
          [ [gcond cond] @ gmovm_opcode mode ~is_load:true
            @ [(rbase_i, 16); (bits, 0)] ]
        )}
    | MOVM (_, _, _) ->
        error node "illegal MOVM operand combination"

    (* Swap *)
    (* case 40:	/* swp oreg,reg,reg */ *)
    (* claude: SWPW/SWPBU, ARM's atomic exchange (deprecated since
     * ARMv6 in favor of LDREX/STREX, but still a real, used
     * instruction -- goken's own runtime uses it for spinlocks,
     * e.g. GO/pkg/runtime/arm/cas5.s: `SWPW (R4), R3`). The 2-operand
     * form (reg_b=None, e.g. `SWPW (R4), R3` from that real example)
     * is the classic atomic-exchange-in-place idiom: the same
     * register (reg_a) is both the new value written to memory (Rm)
     * and the destination that receives the old value read back
     * (Rd). The 3-operand form (reg_b=Some) isn't independently
     * verified against a real fixture -- Rm=reg_a (source, matching
     * the token order: it comes before the indirect operand in
     * Parser_asm5.mly's grammar), Rd=reg_b (destination). *)
    | SWAP (size, (R rn), reg_a, reg_b_opt) ->
        let (rm, rd) = match reg_b_opt with
          | None -> reg_a, reg_a
          | Some reg_b -> reg_a, reg_b
        in
        let (R rm_int) = rm and (R rd_int) = rd in
        let byte_bit = match size with
          | Byte U -> [(1, 22)]
          | Word -> []
          | Byte S | HalfWord _ ->
              raise (Impossible "SWAP only supports Word or Byte U")
        in
        { size = 4; x = None; binary = (fun () ->
          [[gcond cond; (0x2, 23); (0x9, 4)] @ byte_bit
            @ [(rn, 16); (rd_int, 12); (rm_int, 0)]]
        )}

    (* Half words and signed bytes *)
    | MOVE ((HalfWord _ | Byte _), _opt, _from, _dest) -> 
        error node "TODO: half"

    | MOVE (Word, _opt, _from, _dest) ->
       (* stricter: better error message *)
       error node "illegal combination: at least one operand must be a register"

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)
    (* case 10:	/* swi [$con] */ *)
    | SWI i ->
        if i <> 0
        then error node (spf "SWI does not use its parameter under Plan 9/Linux");

        { size = 4; x = None; binary = (fun () ->
          [ [gcond cond; (0xf, 24)] ]
        )}
    (* case 41:	/* rfe -> movm.s.w.u 0(r13),[r15] */ *)
    | RFE ->
        { size = 4; x = None; binary = (fun () -> 
          [ [(0xe8fd8000, 0)] ]
        )}
    (* --------------------------------------------------------------------- *)
    (* Other *)
    (* --------------------------------------------------------------------- *)
    )

(*e: function [[Codegen5.rules]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
(* TODO: could reuse this code and only things changing are the rules to pass?*)
(*s: function [[Codegen5.size_of_instruction]] *)
let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int (* a multiple of 4 *) * pool option =
  let action  = rules env None node in
  action.size, action.x
(*e: function [[Codegen5.size_of_instruction]] *)

(* TODO: could reuse this code and only things changing are the rules to pass?*)
(*s: function [[Codegen5.gen]] *)
let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config) 
  (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in

  (* just for sanity checking *)
  let pc = ref config.init_text in

  cg |> T.iter (fun n ->

    let {size; binary; x = _ }  = 
        rules { Codegen.syms = symbols2; autosize = !autosize }
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
      Logs.app (fun m -> m "%s -->" (Types5.show_instr n.instr));
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.app (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
      Logs.app (fun m -> m ".");
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
(*e: function [[Codegen5.gen]] *)
(*e: Codegen5.ml *)
