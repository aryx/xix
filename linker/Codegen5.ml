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
  | Imsr _ | Ximm _ -> raise (Impossible "should be called only for indirects")
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
    (* claude: old ARM 7500 FP (coprocessor 1) encoding -- goken's
     * case 54 also covers CMPF/CMPD (via `if(p->to.type==D_NONE)
     * rt=0`, since CMP-style instructions put their 2nd operand in
     * the *middle* field, not `to`), handled by our separate CmpF
     * arm just below since our AST already splits that out.
     * Immediate float operands are limited to goken's chipfloats[]
     * (float.c): exactly {0,1,2,3,4,5,0.5,10} -- anything else is a
     * genuine assembler error here (`error node`), not goken's own
     * silent `diag(); rf=0` fallback. *)
    | ArithF ((op, prec), from, middle, (FR rt)) ->
        let r = match middle with Some (FR x) -> x | None -> rt in
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
     * CmpF is already a separate constructor from ArithF. The 2nd
     * float operand goes in the *middle* field (bits[19:16]), same
     * as integer CMP -- see gop_cmp/Cmp above. *)
    | CmpF (_prec, (FR fa), (FR fb)) ->
        { size = 4; x = None; binary = (fun () ->
          [ [gcond cond] @ gop_cmpf @ [(fb, 16); (fa, 0)] ]
        )}

    (* case 74:	/* vfp floating point arith */ *)
    (* case 75:	/* vfp floating point compare */ *)
    (* case 76:	/* vfp floating point fix and float */ *)
    (* claude: NOT ported -- opposite polarity of the armv4/case
     * 22/23/32/33 story above: `vfp = debug['f']` (5l/span.c) is OFF
     * by default, so VFP-flagged optab rows are dead unless goken is
     * invoked with -f, which this harness never does. Case 54
     * (FPA, just above) is what's actually active by default. *)

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
