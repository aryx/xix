(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Either

open Ast_asm
open Ast_asm7

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ARM64/AArch64 code generation, first version.
 *
 * The 'case <n>: ...' comments below refer to goken's linkers/7l/asmout.c
 * (mirrors the Codegen{5,v,i}.ml convention).
 *
 * claude: style note -- unlike Codegen5.ml/Codegenv.ml/Codegeni.ml, which
 * build each instruction word out of several small `Bits.t` tuples (one
 * per field), the helpers here mostly compute the *whole* 32-bit word as
 * a single OCaml int via plain `lor`/`lsl`/`land`, transcribing goken's
 * own C helpers (oprrr/opirr/oaddi/olsr12u/...) close to expression-for-
 * expression, then wrap the result as a single-tuple `Bits.t` at the call
 * site. This is a deliberate choice (not an oversight): AArch64's fields
 * don't decompose as cleanly into goken's own reusable C helper functions
 * as ARM32's did, so following the C shape directly, the same way case
 * 62/63's CASE/BCASE prototype and the MCR/MRC port already did
 * successfully elsewhere in this project, is more faithful and less
 * error-prone here than re-deriving a from-scratch Bits.t decomposition.
 *
 * Scope of this first version (see docs/claude_notes/
 * notes_arm64_port_plan.txt): only 64-bit (X-register, bare-mnemonic)
 * forms are implemented -- the *W-suffixed 32-bit forms (ADDW, MOVW's
 * register-move meaning as opposed to its load/store meaning, etc.) are
 * a real, separate opcode family in goken (a different `sf` bit and, for
 * loads, a different sign-extension story) and are deferred, same
 * "narrower but real" scoping as RISC-V's RV64 *W variants in
 * Codegeni.ml. AND/ORR/EOR/BIC's *immediate* forms (a "bitmask
 * immediate" encoding, genuinely different from ADD/SUB's plain 12-bit
 * uimm) are also deferred -- only their register-register form is
 * implemented; only ADD/SUB support an immediate operand for now.
 * Byte/halfword/32-bit-sign-or-zero-extending moves (B_/H_/W_ in
 * move_size) are declared in the AST already but not implemented yet
 * either. Most importantly: no literal pool yet at all (see Layout7.ml's
 * own comment) -- so "MOV $bigconst,R" and any address-of-global /
 * load-from-global access (which goken's own 7l, confirmed empirically,
 * routes through the *same* literal-pool mechanism as a big plain
 * integer constant -- see Ast_asm7.ml's prelude comment) isn't
 * implemented here yet either; only small ADD-fast-path immediates and
 * plain-register-base (e.g. RSP-relative) memory access are.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error (node : 'a T.node) (s : string) =
  failwith
    (spf "%s at %s on %s" s (T.s_of_loc node.n_loc)
        (Types7.show_instr node.instr))

let w1 (x : int) : Bits.t = [(x land 0xffffffff, 0)]

(*****************************************************************************)
(* Instruction encoding helpers *)
(*****************************************************************************)
(* claude: only the 64-bit ("sf"=1) forms are ever used here -- see this
 * file's prelude comment. *)
let s64 = 1 lsl 31

(* claude: case 1 -- register-register arith base opcodes (goken's
 * oprrr(), the AADD/ASUB/AAND/AORR/AEOR/ABIC rows only). *)
let oprrr_arith (op : arith_opcode) : int =
  match op with
  | ADD -> s64 lor (0x0b lsl 24)
  | SUB -> s64 lor (1 lsl 30) lor (0x0b lsl 24)
  | AND_ -> s64 lor (0xA lsl 24)
  | ORR -> s64 lor (1 lsl 29) lor (0xA lsl 24)
  | EOR -> s64 lor (2 lsl 29) lor (0xA lsl 24)
  | BIC -> s64 lor (0xA lsl 24) lor (1 lsl 21)

(* claude: case 2/4 -- register-immediate ("addcon") base opcodes (goken's
 * opirr(), ADD/SUB rows only -- AND/ORR/EOR/BIC's immediate form uses a
 * different, "bitmask immediate" encoding not implemented here, see this
 * file's prelude comment). *)
let opirr_addsub (op : arith_opcode) : int =
  match op with
  | ADD -> s64 lor (0x11 lsl 24)
  | SUB -> s64 lor (1 lsl 30) lor (0x11 lsl 24)
  | AND_ | ORR | EOR | BIC ->
      failwith "TODO:opirr_addsub AND/ORR/EOR/BIC immediate (bitmask-immediate encoding not implemented)"

(* claude: goken's oaddi() -- packs a 12-bit unsigned immediate (or,
 * shifted left by 12, up to 0xFFF000) into the "addcon" instruction
 * shape. Errors loudly (matching goken's own diag()) rather than
 * silently emitting wrong bytes for anything outside that range --
 * exactly the values case 4's "$addcon" is defined for; anything bigger
 * needs the literal pool, not implemented yet (see this file's prelude
 * comment). *)
let oaddi (node : 'a T.node) (base : int) (v : int) (r : int) (rt : int) : int =
  if v < 0 || v > 0xFFF000
  then error node "TODO: immediate out of ADD/SUB range (needs literal pool, not yet implemented)"
  else if v > 0xFFF then begin
    if v land 0xFFF <> 0
    then error node "TODO: immediate needs both direct and shifted bits (needs literal pool)"
    else base lor (1 lsl 22) lor ((v lsr 12) lsl 10) lor (r lsl 5) lor rt
  end
  else base lor (v lsl 10) lor (r lsl 5) lor rt

(* claude: case 7 -- CMP/CMN, encoded as SUBS/ADDS with an implicit ZR
 * destination (goken's own oprrr()/opirr() alias ACMP->ASUBS and
 * ACMN->AADDS onto the exact same rows, see asmout.c's oprrr/opirr
 * tables) -- kept as separate helpers here since Ast_asm7.cmp_opcode is
 * its own type, not reusing arith_opcode. *)
let oprrr_cmp (op : cmp_opcode) : int =
  match op with
  | CMP -> s64 lor (1 lsl 30) lor (1 lsl 29) lor (0x0b lsl 24) (* SUBS *)
  | CMN -> s64 lor (1 lsl 29) lor (0x0b lsl 24) (* ADDS *)
let opirr_cmp (op : cmp_opcode) : int =
  match op with
  | CMP -> s64 lor (1 lsl 30) lor (1 lsl 29) lor (0x11 lsl 24) (* SUBS $imm *)
  | CMN -> s64 lor (1 lsl 29) lor (0x11 lsl 24) (* ADDS $imm *)

(* claude: case 8 -- shift by immediate, via the bitfield-move family
 * (UBFM/SBFM for LSL/LSR/ASR, EXTR for ROR -- goken's opbfm()/opextr()).
 * `r`/`s` below are UBFM/SBFM's own "immr"/"imms" fields, already
 * pre-transformed by the caller per goken's own case 8 (e.g. LSL's
 * r=(64-v)&63, s=63-v -- not just v directly). *)
let opirr_ubfm = s64 lor (2 lsl 29) lor (0x26 lsl 23) lor (1 lsl 22)
let opirr_sbfm = s64 lor (0x26 lsl 23) lor (1 lsl 22)
let opirr_extr = s64 lor (0x27 lsl 23) lor (1 lsl 22)
let opbfm (base : int) (r : int) (s : int) (rf : int) (rt : int) : int =
  base lor ((r land 0x3F) lsl 16) lor ((s land 0x3F) lsl 10) lor (rf lsl 5) lor rt
let opextr (base : int) (v : int) (rn : int) (rm : int) (rt : int) : int =
  base lor (v lsl 10) lor (rn lsl 5) lor (rm lsl 16) lor rt

(* claude: case 9 -- shift by register (LSLV/LSRV/ASRV/RORV), goken's
 * `OPDP2(x) = 0<<30 | 0<<29 | 0xd6<<21 | (x)<<10`. Same (rf<<16)|(r<<5)|rt
 * operand shape as case 1's oprrr_arith. *)
let opdp2 (x : int) : int = (0xd6 lsl 21) lor (x lsl 10)
let oprrr_shift (op : shift_opcode) : int =
  match op with
  | LSL -> s64 lor opdp2 8
  | LSR -> s64 lor opdp2 9
  | ASR -> s64 lor opdp2 10
  | ROR -> s64 lor opdp2 11

(* claude: case 15's simple (no from3/accumulate) MUL Rm,[Rn,]Rd, an
 * alias of MADD with Ra=ZR -- goken's `oprrr(AMUL) = S64 | 0<<29 |
 * 0x1B<<24 | 0<<21 | 0<<15`. *)
let oprrr_mul = s64 lor (0x1B lsl 24)

(* claude: case 5/6 -- unconditional branch/call. `opbra(AB/ABL)` for the
 * direct-label form (imm26 field, packed by the caller), `opbrr()`
 * (goken's OPBLR macro) for the indirect-through-register form, which
 * also directly gives RET's own encoding (x=2) -- ARM64 genuinely has a
 * real RET instruction sharing this same instruction family (BR/BLR/RET
 * only differ in this 2-bit field), unlike every other arch ported so
 * far where RET is purely a synthesized virtual instr. *)
let opbra_b = 5 lsl 26 (* B: sf-like bit clear *)
let opbra_bl = s64 lor (5 lsl 26) (* BL *)
let opblr (x : int) : int = (0x6B lsl 25) lor (x lsl 21) lor (0x1F lsl 16)
let opbrr_b = opblr 0 (* BR *)
let opbrr_bl = opblr 1 (* BLR *)
let opbrr_ret = opblr 2 (* RET *)

(* claude: case 7 (branch variant) -- BEQ/BNE/...; goken's OPBcc(x) =
 * 0x2A<<25 | (x&15), the condition packed into a distinct base opcode
 * from the branch's own PC-relative imm19 field (ORed in by the caller
 * via `<<5`, see branch_delta_19 below). *)
let opbcc (cond : int) : int = (0x2A lsl 25) lor (cond land 0xF)
let int_of_condition (c : condition) : int =
  match c with
  | EQ -> 0x0 | NE -> 0x1
  | GE U -> 0x2 | LT U -> 0x3
  | MI -> 0x4 | PL -> 0x5
  | VS -> 0x6 | VC -> 0x7
  | GT U -> 0x8 | LE U -> 0x9
  | GE S -> 0xa | LT S -> 0xb
  | GT S -> 0xc | LE S -> 0xd
  | AL -> 0xe

(* claude: case 8's own (distinct) LTYPE -- CBZ/CBNZ Rt,label. Goken's
 * `0x1A<<25 | (nz)<<24`, the imm19 branch field ORed in at bit5 by the
 * caller, same as opbcc above. *)
let opcbz (nonzero : bool) : int = (0x1A lsl 25) lor ((if nonzero then 1 else 0) lsl 24)

(* claude: case 10 -- SVC (goken's opimm(), the ASVC row only; BRK/HVC/
 * HLT/etc share this LTYPE but aren't wired -- see Ast_asm7.ml). *)
let opimm_svc = (0xD4 lsl 24) lor 1

(* claude: case 20/21 -- MOV(64-bit only, "AMOV" in goken) load/store,
 * scaled-12-bit-unsigned-offset form only (goken's `v >= 0` branch of
 * case 20/21; the `v < 0` "unscaled 9-bit signed" branch, and the pre/
 * post-increment "!" forms of case 22/23, aren't implemented yet).
 * goken's `LDSTR12U(sz,v,opc) = sz<<30 | 7<<27 | v<<26 | 1<<24 | opc<<22`
 * with (sz=3,v=0,opc=1) for AMOV's *load*; store is the same opcode with
 * opc's bit cleared (goken's `LD2STR`, `o & ~(3<<22)`). *)
let ldstr12u (sz : int) (v : int) (opc : int) : int =
  (sz lsl 30) lor (7 lsl 27) lor (v lsl 26) lor (1 lsl 24) lor (opc lsl 22)
let opldr12_mov = ldstr12u 3 0 1
let opstr12_mov = opldr12_mov land (lnot (3 lsl 22))
let olsr12u (node : 'a T.node) (base : int) (v : int) (b : int) (r : int) : int =
  if v < 0 || v >= (1 lsl 12)
  then error node "TODO: offset out of 12-bit scaled range (needs literal pool / unscaled form, not yet implemented)"
  else base lor ((v land 0xFFF) lsl 10) lor (b lsl 5) lor r

(* claude: case 22/23 -- pre/post-index writeback load/store ("MOV
 * Rt,-16(Rbase)!" / "MOV Rt,(Rbase)16!"), goken's `opldrpp()`
 * (AMOV row: `3<<30 | 7<<27 | 0<<26 | 0<<24 | 1<<22`, LD2STR clears
 * the opc field for the store direction) plus a `v<<12` signed 9-bit
 * offset and a mode field at bits[11:10] (goken: `1<<10` for
 * post-index, `3<<10` for pre-index). Needed for Rewrite7.ml's
 * RETURN-expansion link-register save/restore. *)
let opldrpp_mov = (3 lsl 30) lor (7 lsl 27) lor (1 lsl 22)
let opstrpp_mov = opldrpp_mov land (lnot (3 lsl 22))
let ldrstr_pp (node : 'a T.node) (base : int) (is_post : bool) (v : int) (b : int) (r : int) : int =
  if v < -256 || v > 255
  then error node "TODO: pre/post-index offset out of 9-bit signed range"
  else
    let mode_bits = if is_post then 1 else 3 in
    base lor (mode_bits lsl 10) lor ((v land 0x1FF) lsl 12) lor (b lsl 5) lor r

(* claude: case 32 -- "MOV $con,R -> movz/movn". A plain integer literal
 * for a MOV pseudo-op goes through goken's full `aclass()`/`cmp()`
 * constant-classification chain (span.c), NOT just a direct movcon()
 * check as this file's first attempt assumed (caught the hard way: "MOV
 * $42,R0" assembles to a real MOVZ, but "MOV $7,R0" -- a value that
 * ALSO trivially fits one 16-bit lane -- assembles to a literal-pool
 * LDR instead, confirmed directly against goken; the difference is
 * that 7 is also a valid ARM64 "logical immediate" bit pattern
 * (`isbitcon`, a contiguous run of 1-bits, possibly rotated/replicated)
 * while 42 isn't, and aclass() checks that classification *before* ever
 * considering movcon() -- see isbitcon's own comment below). The real
 * priority chain (span.c's aclass() D_CONST case, cross-referenced with
 * cmp()'s C_MOVCON compatibility list, which only accepts C_ZCON/
 * C_ADDCON0, not the more general C_ADDCON/C_ABCON):
 *  1. v = 0 -> MOVZ #0 (trivially lane 0).
 *  2. isaddcon(v) [fits ADD/SUB's own "$addcon" 12-bit-optionally-
 *     shifted-by-12 immediate shape -- same range check as oaddi above]:
 *     - isbitcon(v) -> needs the pool (C_ABCON, not movcon-compatible).
 *     - v <= 0xFFF -> MOVZ, lane 0 (C_ADDCON0, the one addcon subclass
 *       cmp() does accept for C_MOVCON).
 *     - else (the "shifted by 12" ADDCON case) -> needs the pool
 *       (C_ADDCON itself isn't movcon-compatible either).
 *  3. else, movcon(v) -- v fits entirely within one 16-bit lane, the
 *     other 3 all zero -> MOVZ at that lane.
 *  4. else, movcon(lnot v) -- v's bitwise complement fits one lane
 *     (the other 3 lanes of v are all ones) -> MOVN (loads the
 *     complement) at that lane.
 *  5. else -> needs the literal pool (case 12), not implemented yet.
 *)
let isaddcon (v : int) : bool =
  v >= 0 && (if v land 0xFFF = 0 then v asr 12 else v) <= 0xFFF

(* claude: ARM64's "logical immediate" bitmask-pattern test (goken's
 * `isbitcon`/`findmask`, span.c) -- is `v` some rotation of a
 * contiguous run of 1-bits, replicated at some power-of-two element
 * size (2/4/8/16/32/64) that evenly divides the register width? Ported
 * as a direct search rather than goken's closed-form `findmask` (which
 * relies on x86-style bit-scan intrinsics not worth reproducing here)
 * -- same answer, just less clever. The e=64 (no replication) case is
 * checked directly as "v equals some contiguous run of 1..63 ones at
 * some bit position" rather than via 64-bit rotation, since OCaml's
 * native int is 63 bits and every constant this port deals with is a
 * small nonnegative value with no bits anywhere near bit 63 -- a
 * wrapping rotation could only ever matter for a value with high bits
 * set, which doesn't arise here. Verified directly against goken for
 * several values (7/15/3/1/0xff -> bitcon, pool; 42/100/5 -> not
 * bitcon, MOVZ) before trusting this. *)
let isbitcon (v : int) : bool =
  let is_contig_run_e64 (v : int) : bool =
    if v <= 0 then false
    else
      let rec try_n n =
        if n >= 63 then false
        else
          let pat = (1 lsl n) - 1 in
          let rec try_k k =
            if k > 62 - n then false
            else if pat lsl k = v then true
            else try_k (k + 1)
          in
          if try_k 0 then true else try_n (n + 1)
      in try_n 1
  in
  let rotate_right_e (p : int) (e : int) (r : int) : int =
    let mask = (1 lsl e) - 1 in
    let p = p land mask in
    if r = 0 then p else ((p lsr r) lor (p lsl (e - r))) land mask
  in
  let is_contig_ones (p : int) (e : int) : bool =
    let mask = (1 lsl e) - 1 in
    let p = p land mask in
    if p = 0 || p = mask then false
    else
      let rec try_r r =
        if r >= e then false
        else if rotate_right_e p e r land (rotate_right_e p e r + 1) = 0
        then true
        else try_r (r + 1)
      in try_r 0
  in
  let rec try_e e =
    if e >= 64 then is_contig_run_e64 v
    else
      let low = v land ((1 lsl e) - 1) in
      let reps = 64 / e in
      let full =
        let rec build acc n = if n = 0 then acc else build ((acc lsl e) lor low) (n - 1) in
        build 0 reps
      in
      let checkbits = min 62 (e * reps) in
      let vmask = (1 lsl checkbits) - 1 in
      if full land vmask = v land vmask && is_contig_ones low e
      then true
      else try_e (e * 2)
  in try_e 2

let movcon (v : int) : int option =
  let rec aux s =
    if s >= 4 then None
    else if v land (lnot (0xFFFF lsl (s * 16))) = 0 then Some s
    else aux (s + 1)
  in aux 0
let opirr_movz = s64 lor (2 lsl 29) lor (0x25 lsl 23)
let opirr_movn = s64 lor (0x25 lsl 23)

(* claude: the full classification chain (see the block comment above)
 * -- `None` means "needs the literal pool, not implemented yet". The
 * result still needs `rt` (bits[4:0]) ORed in by the caller. *)
let move_immediate_encoding (v : int) : int option =
  if v = 0 then Some opirr_movz
  else if isaddcon v then
    (if isbitcon v || v > 0xFFF then None
     else Some (opirr_movz lor (v lsl 5)))
  else
    match movcon v with
    | Some s -> Some (opirr_movz lor (((v asr (s*16)) land 0xFFFF) lsl 5) lor (s lsl 21))
    | None ->
        (match movcon (lnot v) with
        | Some s ->
            let d = lnot v in
            Some (opirr_movn lor (((d asr (s*16)) land 0xFFFF) lsl 5) lor (s lsl 21))
        | None -> None)

(* claude: case 24 -- register-to-register "MOV Rs,Rd", a real AArch64
 * pseudo-op for either ORR Rs,ZR,Rd (the common case) or ADD $0,Rs,Rd
 * (used instead whenever SP is involved on either side, since ORR can't
 * reference SP as a source/dest register in the real ISA -- goken's own
 * `s = rf==REGSP || rt==REGSP` check). *)
let gmov_reg_reg (rf_i : int) (rt_i : int) : int =
  if rf_i = 31 || rt_i = 31 (* RSP/ZR share register 31 -- see Ast_asm7.ml's
                             * prelude comment; this reuses the same
                             * SP-aware branch goken's case 24 takes for
                             * ZR too, which is harmless since ORR
                             * Rzr,ZR,Rd and ADD $0,Rzr,Rd compute the
                             * same result either way *)
  then (opirr_addsub ADD) lor (rf_i lsl 5) lor rt_i (* ADD $0,Rf,Rt *)
  else (oprrr_arith ORR) lor (rf_i lsl 16) lor (31 lsl 5) lor rt_i (* ORR Rf,ZR,Rt *)

(* claude: branch offset -- unlike ARM32 (PC = current instr + 8, a
 * classic 3-stage-pipeline quirk) AArch64 PC-relative fields are always
 * relative to the branch/CBZ/Bcc instruction's *own* address, no bias at
 * all. `real_pc` is already absolute (Layout7.ml's `pc := ref init_text`)
 * so no separate +INITTEXT term is needed either, same reasoning as
 * Codegeni.ml's (RISC-V) branch_delta. *)
(* claude: caught a real bug the hard way (byte-diff against goken):
 * AArch64's PC-relative branch immediates are *word*-scaled (goken's
 * own `brdist(..., shift=2)` -- every field here is `(target-pc)>>2`,
 * not the raw byte delta), since every instruction is 4-byte aligned.
 * This first version forgot the `>>2` entirely, sending every non-
 * adjacent branch target 4x too far. All real_pc values are absolute
 * and always a multiple of 4, so a plain arithmetic shift is exact and
 * sign-preserving for backward branches. *)
let branch_delta (node : 'a T.node) : int =
  match node.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst -> (ndst.real_pc - node.real_pc) asr 2

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)

let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  ignore init_data;
  match node.instr with
  (* Reusable *)
  | T.Virt _ | T.TEXT _ | T.WORD _ ->
      Codegen.default_rules env init_data node

  | T.I instr ->
    (match instr with

    (* --------------------------------------------------------------------- *)
    (* Arithmetic *)
    (* --------------------------------------------------------------------- *)

    (* case 1: op Rm,[Rn,]Rd *)
    | Arith (op, Reg (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_arith op lor (rf lsl 16) lor (r lsl 5) lor rt) ]
        )}
    (* case 2/4: op $imm,[Rn,]Rd ("addcon" fast path only -- see oaddi) *)
    | Arith (op, Imm i, middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oaddi node (opirr_addsub op) i r rt) ]
        )}

    (* case 8: shift by immediate (bitfield-move encoding) *)
    | Shift (op, Imm v, middle, (R rt)) ->
        let (R rf) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (match op with
            | LSL -> opbfm opirr_ubfm ((64 - v) land 63) (63 - v) rf rt
            | LSR -> opbfm opirr_ubfm v 63 rf rt
            | ASR -> opbfm opirr_sbfm v 63 rf rt
            | ROR -> opextr opirr_extr v rf rf rt
          ) ]
        )}
    (* case 9: shift by register *)
    | Shift (op, Reg (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_shift op lor (rf lsl 16) lor (r lsl 5) lor rt) ]
        )}

    (* case 7: CMP/CMN *)
    | Cmp (op, Reg (R rf), (R rn)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_cmp op lor (rf lsl 16) lor (rn lsl 5) lor 31) ]
        )}
    | Cmp (op, Imm i, (R rn)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oaddi node (opirr_cmp op) i rn 31) ]
        )}

    (* case 15: simple 3-operand MUL Rm,[Rn,]Rd (no accumulate) *)
    | ArithMul ((R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_mul lor (rf lsl 16) lor (31 lsl 10) lor (r lsl 5) lor rt) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Memory / Move *)
    (* --------------------------------------------------------------------- *)

    (* case 24: MOV Rs,Rd (register-to-register) *)
    | Move (X_, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (gmov_reg_reg rf rt) ]
        )}

    (* case 32: MOV $con,Rd -> movz/movn (see move_immediate_encoding's
     * own comment; no literal pool yet for anything that needs it) *)
    | Move (X_, Right (Int i), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (match move_immediate_encoding i with
            | Some base -> base lor rt
            | None ->
                error node "TODO: constant needs the literal pool (not yet implemented)")
          ] )}

    (* case 20: MOV Rs,O(Rbase) -> STR (scaled 12-bit unsigned offset only) *)
    | Move (X_, Left (GReg (R rf)), Indirect ((R rbase), offset)) ->
        if offset mod 8 <> 0
        then error node "TODO: unaligned/unscaled store offset (not yet implemented)"
        else
          { size = 4; x = None; binary = (fun () ->
            [ w1 (olsr12u node opstr12_mov (offset / 8) rbase rf) ]
          )}
    (* case 21: MOV O(Rbase),Rd -> LDR (scaled 12-bit unsigned offset only) *)
    | Move (X_, Left (Indirect ((R rbase), offset)), GReg (R rt)) ->
        if offset mod 8 <> 0
        then error node "TODO: unaligned/unscaled load offset (not yet implemented)"
        else
          { size = 4; x = None; binary = (fun () ->
            [ w1 (olsr12u node opldr12_mov (offset / 8) rbase rt) ]
          )}

    (* case 23: MOV Rf,-16(Rbase)! / MOV Rf,(Rbase)16! -- pre/post-index
     * writeback store *)
    | Move (X_, Left (GReg (R rf)), PreIndex ((R rbase), offset)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opstrpp_mov false offset rbase rf) ]
        )}
    | Move (X_, Left (GReg (R rf)), PostIndex ((R rbase), offset)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opstrpp_mov true offset rbase rf) ]
        )}
    (* case 22: MOV -16(Rbase)!,Rt / MOV (Rbase)16!,Rt -- pre/post-index
     * writeback load *)
    | Move (X_, Left (PreIndex ((R rbase), offset)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opldrpp_mov false offset rbase rt) ]
        )}
    | Move (X_, Left (PostIndex ((R rbase), offset)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opldrpp_mov true offset rbase rt) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* case 5: B label / BL label *)
    | B { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbra_b lor ((branch_delta node) land 0x3FFFFFF)) ]
        )}
    | BL { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbra_bl lor ((branch_delta node) land 0x3FFFFFF)) ]
        )}
    (* case 6: B (Rn) / BL (Rn) -- indirect through register *)
    | B { contents = (IndirectJump (R rt)) } ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_b lor (rt lsl 5)) ]) }
    | BL { contents = (IndirectJump (R rt)) } ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_bl lor (rt lsl 5)) ]) }

    (* case 7 (branch variant): BEQ label etc *)
    | Bxx (cond, { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbcc (int_of_condition cond)
                lor (((branch_delta node) land 0x7FFFF) lsl 5)) ]
        )}

    (* case 8's own LTYPE: CBZ/CBNZ Rt,label *)
    | CBxx (nonzero, (R rt), { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opcbz nonzero
                lor (((branch_delta node) land 0x7FFFF) lsl 5)
                lor rt) ]
        )}

    (* RET / RET Rn -- defaults to RLINK (X30), goken's own default for
     * a bare "RET". *)
    | RET None ->
        { size = 4; x = None; binary = (fun () ->
          let (R rt) = rLINK in [ w1 (opbrr_ret lor (rt lsl 5)) ]
        )}
    | RET (Some (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_ret lor (rt lsl 5)) ]) }

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* case 10: SVC [$con] -- goken's own case ignores the immediate's
     * actual value for the Linux syscall ABI purpose (syscall number is
     * in R8, not encoded in the instruction) but still packs it into the
     * instruction if given, same as ARM32/MIPS's SWI/SYSCALL. *)
    | SVC i ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opimm_svc lor ((i land 0xffff) lsl 5)) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Not yet implemented -- see this file's prelude comment *)
    (* --------------------------------------------------------------------- *)
    | Move _ ->
        error node "Codegen7: TODO: move operand combination not handled yet"
    | B { contents = (Relative _ | LabelUse _ | SymbolJump _) }
    | BL { contents = (Relative _ | LabelUse _ | SymbolJump _) }
    | Bxx (_, { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) })
    | CBxx (_, _, { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) }) ->
        raise (Impossible
          "branch operand should have been resolved to Absolute by now")
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int =
  let action = rules env None node in
  action.size

let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in
  let pc = ref config.init_text in

  cg |> T.iter (fun n ->
    let {size; binary; x = _} =
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
      Logs.app (fun m -> m " %.8x: %s"
                 !pc
                  (xs |> List.map (fun x -> spf "%.8x" (Bits.int_of_bits32 x))
                      |> String.concat " "));
    end;

    let xs = xs |> List.map Bits.int_of_bits32 in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten
