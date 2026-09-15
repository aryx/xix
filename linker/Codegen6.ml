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

module A = Ast_asm
module T = Types
open Ast_asm6

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* amd64 codegen -- REX/ModRM/SIB/immediate byte encoding, ported from
 * goken's real ~/goken/linkers/6l/span.c (doasm()/asmandsz()/asmand()),
 * grounded throughout against real 6a/6l byte output (see
 * docs/claude_notes/amd64_port.md) rather than derived from
 * reading span.c alone, since x86-64's encoding table (optab.c) is by
 * far the densest of any arch ported so far.
 *
 * claude: THE central departure from every other arch's Codegen*.ml:
 * amd64 instructions are variable-length (1-15 bytes), not a fixed
 * 4-byte word, so this file builds a plain `int list` of raw bytes
 * (0-255 each) directly, instead of the shared `Bits.int32`/32-bit-word
 * bitfield convention every other arch uses (that convention's
 * `sanity_check_32` hardcodes a 32-bit budget -- see Bits.ml -- so it
 * doesn't fit here at all). `size`, correspondingly, is a byte count,
 * not "a multiple of 4". See linker/Types.ml's `bytes_of_words` comment
 * for how the shared executable writer was generalized to accept this.
 *
 * Scope (see Ast_asm6.ml's own prelude and amd64_port.md for the full
 * writeup): every operand-class combination this port's own fixtures
 * need is implemented, including every ordinary register (BP/R13
 * included, see `encode_rm`'s own comment for their real mod=00-means-
 * RIP-relative quirk) as a memory base, and full SIB scaled-index
 * addressing (`RMemIndexed`/`RAbsIndexed`, REX.X included) -- every
 * unhandled shape still raises Todo rather than silently emitting
 * wrong bytes (same convention as every other arch's own "not wired
 * yet" gaps). Real, deliberate gaps that remain:
 *  - Arith/Cmp/Test/CmpXchg's own immediate forms don't cover every
 *    real x86 immediate-group opcode variant (e.g. Arith/Cmp's own
 *    imm32 form, `0x81`, IS wired; IMUL's 3-operand immediate forms
 *    aren't).
 *  - R12 as a memory base (needs the same mandatory-SIB quirk as SP,
 *    plain or scaled-index alike) isn't wired -- no real closure
 *    stress-tested so far needs it.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type action = {
  size: int; (* a byte count -- NOT "a multiple of 4", see prelude *)
  binary: unit -> int list; (* raw bytes, 0-255 each *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: register *encoding* number (0-15), already exactly goken's
 * own reg[]/regrex[] ModRM/REX-ready values -- see Ast_asm6.ml's
 * prelude for why registers are numbered this way in this port. *)
let reg_num (A.R i) = i
(* claude: XMM registers are numbered identically to GP ones (see
 * `gen_of_xgen`'s own comment below) -- a separate accessor only
 * because `xregister` is its own OCaml type, not a version-mismatch in
 * the underlying encoding. *)
let xreg_num (X i) = i

let modrm ~md ~reg ~rm = ((md land 3) lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7)
let sib ~scale ~index ~base = ((scale land 3) lsl 6) lor ((index land 7) lsl 3) lor (base land 7)

let le16 (v : int) : int list =
  [ v land 0xff; (v asr 8) land 0xff ]

let le32 (v : int) : int list =
  [ v land 0xff; (v asr 8) land 0xff; (v asr 16) land 0xff; (v asr 24) land 0xff ]

let le64 (v : int) : int list =
  le32 v @ le32 (v asr 32)

(* claude: a `gen` operand, resolved to its final addressing-mode shape
 * -- `Entity`'s two real cases (SB-relative global, FP-relative local)
 * both collapse into one of these two once the linker's symbol table
 * (for `Global`) or the enclosing TEXT's frame size (for `Local`) is
 * known, matching every other arch's own Local/Param-resolution
 * pattern (e.g. Codegen5.ml's `base_and_offset_of_indirect`). *)
type resolved_gen =
  | RReg of A.register
  | RMem of A.register * int (* base register (always SP here -- see
                               * this file's prelude), displacement *)
  | RAbs of int (* absolute virtual address (SB-relative global) *)
  (* claude: real x86 SIB scaled-index addressing -- see Ast_asm6.ml's
   * IndirectScaled/EntityScaled comment. `RMemIndexed`'s base is a real
   * register (SIB.base); `RAbsIndexed`'s base is absent (SIB.base=101,
   * no-base, disp32-only, same convention as `RAbs` above), used for a
   * scaled index off an SB-relative global. *)
  | RMemIndexed of A.register * int * A.register * int (* base, offset, index, scale *)
  | RAbsIndexed of int * A.register * int (* absolute address, index, scale *)

(* claude: REX prefix -- 0x40 | W<<3 | R<<2 | X<<1 | B. Ported from
 * goken's own obj.c reg[]/regrex[] table init (span.c/obj.c: register
 * *encoding* is `(i - D_AX) & 7` uniformly for AX..DI *and* R8..R15 --
 * already handled by `reg_num` returning the raw 0-15 value and
 * `modrm`/`sib` masking with `land 7` -- but R8-R15 additionally set
 * Rxr|Rxx|Rxb in regrex[], meaning "this register may need any of
 * REX.R/.X/.B depending on which ModRM/SIB field it ends up in"). X
 * (SIB index) is always 0 (no indexed addressing implemented, see
 * prelude); R comes from whichever register sits in ModRM.reg
 * (`reg_field` below -- for the immediate-group opcodes this is a
 * fixed 0-7 opcode-extension digit, never actually a register, so it
 * never contributes); B comes from whichever register sits in
 * ModRM.rm or SIB.base (`rex_b_of_resolved_gen` below -- RAbs's fixed
 * no-base SIB pattern never contributes either).
 *
 * claude: for a 32-bit (L_) instruction the REX byte is *entirely
 * optional* -- 32-bit is the default operand size in long mode, so W
 * is 0 and, confirmed against real 6a ("ADDL BX,AX" assembles to just
 * "01 d8", no prefix at all), the whole byte is omitted when R/B are
 * also both 0. Returns a 0-or-1-element list so callers can just `@`
 * it in either case. *)
(* claude: split into 3 arms (rather than one shared "Ctor1 (A.R n)
 * | Ctor2 (A.R n, _) | Ctor3 (A.R n, _, _, _) -> ..." or-pattern) --
 * ocaml-light's own pattern-matcher rejects a variable bound in more
 * than one or-pattern alternative ("This variable is bound several
 * times in this matching"), unlike modern OCaml which allows it (see
 * the top-level CLAUDE.md's ocaml-light compatibility constraint). *)
let rex_b_of_resolved_gen = function
  | RReg (A.R n) -> if n >= 8 then 1 else 0
  | RMem (A.R n, _) -> if n >= 8 then 1 else 0
  | RMemIndexed (A.R n, _, _, _) -> if n >= 8 then 1 else 0
  | RAbs _ | RAbsIndexed _ -> 0

(* claude: REX.X (SIB.index's own high bit) -- the scaled-index sibling
 * of `rex_b_of_resolved_gen` above, needed now that a `resolved_gen`
 * can carry a real index register. 0 for every non-indexed case (no
 * SIB byte, so no index field to extend). Not reachable with an
 * R8-R15 index by any real closure stress-tested so far, but wired for
 * correctness rather than left as a silent gap, same as B/R above. *)
let rex_x_of_resolved_gen = function
  | RReg _ | RMem _ | RAbs _ -> 0
  | RMemIndexed (_, _, A.R n, _) -> if n >= 8 then 1 else 0
  | RAbsIndexed (_, A.R n, _) -> if n >= 8 then 1 else 0

(* claude: goken's own regrex[D_SPB..D_DIB] = 0x40 quirk (obj.c) --
 * real amd64 ModRM/opcode-embedded register field values 4-7, when
 * accessed at *byte* width with *no* REX byte present at all, name the
 * legacy high-byte registers AH/CH/DH/BH instead of the low bytes of
 * SP/BP/SI/DI; a REX byte (even an otherwise-empty 0x40) switches the
 * same field values 4-7 over to meaning SPL/BPL/SIL/DIL. This port's
 * register model has no separate AH/BH/CH/DH token at all (see
 * Ast_asm6.ml's `width` comment) -- whenever the grammar names SP/BP/
 * SI/DI (indices 4-7) in a B_-width instruction, it always means the
 * *low* byte, so a REX byte must be forced even when otherwise empty,
 * exactly matching goken's own bytereg()+regrex forcing. R8-R15
 * (indices 8-15) already force a REX byte for their own extension-bit
 * reasons regardless of width, so only 4-7 needs a special case here.
 *
 * claude: crucially, this quirk is about a *register holding a value*
 * (ModRM.reg, or ModRM.rm when mod=11 -- register-direct), never about
 * a register used as a *memory addressing base* (mod!=11, e.g. "-8(SP)"
 * -- there is no "high byte of the address in SP" ambiguity at all, so
 * `rm`'s own contribution below only ever checks the `RReg` case, not
 * `RMem`), and never about a fixed *opcode-extension digit* sharing
 * the same 0-7 numeric range as a register field would (e.g. SUB's
 * ext=5, XOR's ext=6, CMP's ext=7 -- all coincidentally in 4-7, but
 * meaning "the literal immediate-group sub-opcode", not "register
 * SP/BP/SI/DI") -- confirmed the hard way: an earlier version of this
 * check applied `regrex_forces_rex` to every `reg_field` unconditionally,
 * which broke "SUBB $1,BX"/"CMPB BX,$5" (real 6a/6l: "80 eb 01"/
 * "80 fb 05", no REX at all) and "MOVB AX,-8(SP)" (real 6a/6l:
 * "88 44 24 f8", no REX -- SP here is a memory base, not a value
 * register) by spuriously forcing a REX byte. `reg_is_register` lets
 * the two immediate-group call sites (Arith/Cmp's own B_ immediate
 * clauses, where `reg_field` is always a fixed ext digit) opt out. *)
let regrex_forces_rex (n : int) : bool = n >= 4 && n <= 7

let rex_opt ~(reg_is_register : bool) ~(width : width) ~(reg_field : int)
    ~(rm : resolved_gen) : int list =
  let w = match width with Q_ -> 8 | L_ | W_ | B_ -> 0 in
  let r = if reg_field >= 8 then 4 (* Rxr *) else 0 in
  let x = rex_x_of_resolved_gen rm lsl 1 in
  let b = rex_b_of_resolved_gen rm in
  let forced =
    width = B_ &&
    ((reg_is_register && regrex_forces_rex reg_field)
     || (match rm with
         | RReg (A.R n) -> regrex_forces_rex n
         | RMem _ | RAbs _ | RMemIndexed _ | RAbsIndexed _ -> false))
  in
  if w <> 0 || r <> 0 || x <> 0 || b <> 0 || forced
  then [ 0x40 lor w lor r lor x lor b ]
  else []

(* claude: goken's own "Pe" prefix (0x66, operand-size override) for a
 * 16-bit (W_) instruction -- confirmed against real 6a it comes
 * *before* any REX byte ("MOVW AX,R9" -> "66 41 89 c1"), matching real
 * x86's own prefix-ordering rule (legacy prefixes precede REX, which
 * must immediately precede the opcode). Q_/L_/B_ need no such prefix
 * (B_'s own "Pb" isn't a real prefix byte at all -- see
 * `regrex_forces_rex` above and Ast_asm6.ml's `width` comment). *)
let prefix66 (width : width) : int list =
  match width with W_ -> [0x66] | Q_ | L_ | B_ -> []

(* claude: `init_data` is `None` here (as opposed to `resolve_gen_full`
 * below) -- raises Impossible on any `Entity`/`EntityScaled (A.Global
 * ...)` reaching a SData2 (data-segment) symbol, since resolving one
 * needs init_data. `rules` below now calls `resolve_gen_full`
 * unconditionally instead of this simpler version -- real closures
 * (stress-testing lib_core/libc) turned out to reach a data-segment
 * global through almost every instruction shape (Extend, Unary,
 * Arith's Mem source, Cmp's Zr_m row, ...), not just Move's own src/
 * dst, so there was no instruction-shape-based way left to tell in
 * advance which callers could safely skip init_data. Kept as the
 * lower-level primitive `resolve_gen_full` itself still falls back to
 * (see its own `| _ -> resolve_gen ...` case) for every non-Global
 * `gen`. *)
(* claude: `Entity (A.Global ...)`'s own SText2-only resolution, shared
 * with `EntityScaled`'s analogous case below -- see `resolve_gen`'s own
 * comment for why SData2 needs `resolve_gen_full` instead. *)
let resolve_global_text_addr (env : Codegen.env) (node : 'a T.node)
    (glob : A.global) (off : A.offset) : int =
  match Hashtbl.find env.syms (T.symbol_of_global glob) with
  | T.SText2 real_pc -> real_pc + off
  | T.SData2 _ ->
      raise (Impossible
        (spf "amd64: a SData global needs init_data -- use \
              resolve_gen_full, not resolve_gen, at %s"
              (T.s_of_loc node.T.n_loc)))

(* claude: `Entity (A.Local ...)`'s own FP-relative resolution, shared
 * with `EntityScaled`'s analogous case below. *)
let resolve_local_offset (env : Codegen.env) (off : A.offset) : int =
  (* claude: "buf+0(FP)" -- see Ast_asm6.ml's prelude for why this
   * is the FP-relative *virtual* addressing convention (unlike
   * bare "SP", which this arch's own grammar never routes through
   * Entity at all). The "+8" bias is amd64-specific: goken's real
   * CALL pushes an 8-byte return address onto the stack in
   * hardware (no link-register save to synthesize, unlike every
   * RISC arch ported so far), so by the time a callee's own body
   * runs, its first parameter sits 8 bytes above wherever its own
   * (already-fully-adjusted, see env.autosize) SP points --
   * confirmed against goken's real 6a/6l byte output for
   * hello_linux_amd64.s's own write(buf,len) (autosize=0 there, so
   * FP+0 resolves to exactly SP+8, matching the real bytes:
   * "MOVQ buf+0(FP),SI" assembles to "48 8b 74 24 08", i.e.
   * "mov rsi,[rsp+8]"). *)
  env.autosize + 8 + off

(* claude: `LocalSP`/`LocalSPScaled`'s own resolution -- a *named*
 * local against SP (e.g. real fmt/vfprint.c's own "f+-104(SP)") is
 * goken's own "pseudo-SP" convention (the same one Go's own assembler
 * is famous for): "name+N(SP)" addresses N bytes above the TOP of the
 * local frame, i.e. `autosize + N` -- genuinely different from the
 * bare, unlabeled "N(SP)" form (`Indirect`/`IndirectScaled`, no
 * autosize bias at all, used only for marshaling an outgoing call's
 * own arguments at the bottom of the frame). No "+8" here, unlike
 * `resolve_local_offset` above -- this is SP-relative, not FP-
 * relative, so there's no return-address bias to add. Confirmed
 * against real 6a/6l: vfprint's own real $400 frame, "f+-104(SP)" ->
 * "lea 0x128(%rsp)" (400+(-104)=296=0x128), "buf+-360(SP)" ->
 * "lea 0x28(%rsp)" (400+(-360)=40=0x28). Found stress-testing real
 * lib_core/libc -- this port's earlier version used the raw,
 * unadjusted offset directly, which happened to still produce
 * syntactically valid (if wrong) bytes for every smaller fixture
 * tested so far, corrupting the stack (and eventually crashing at
 * runtime, nowhere near the actual bug) only once a real closure hit
 * a function whose own local's true hardware offset differed enough
 * from its raw source-level offset to overlap live data. *)
let resolve_local_sp_offset (env : Codegen.env) (off : A.offset) : int =
  env.autosize + off

let resolve_gen (env : Codegen.env) (node : 'a T.node) (g : gen) : resolved_gen =
  match g with
  | GReg r -> RReg r
  | Indirect (r, off) -> RMem (r, off)
  | Entity (A.Global (glob, off)) -> RAbs (resolve_global_text_addr env node glob off)
  | Entity (A.Local (_, off)) -> RMem (rSP, resolve_local_offset env off)
  | Entity (A.Param _) ->
      raise (Impossible
        (spf "amd64 grammar never constructs Entity(Param) -- see \
              Ast_asm6.ml's prelude (bare SP is a real register here, \
              not a virtual pseudo-register) at %s" (T.s_of_loc node.T.n_loc)))
  | IndirectScaled (r, off, idx, scale) -> RMemIndexed (r, off, idx, scale)
  | EntityScaled (A.Global (glob, off), idx, scale) ->
      RAbsIndexed (resolve_global_text_addr env node glob off, idx, scale)
  | EntityScaled (A.Local (_, off), idx, scale) ->
      RMemIndexed (rSP, resolve_local_offset env off, idx, scale)
  | EntityScaled (A.Param _, _, _) ->
      raise (Impossible
        (spf "amd64 grammar never constructs EntityScaled(Param) -- see \
              Ast_asm6.ml's prelude (bare SP is a real register here, \
              not a virtual pseudo-register) at %s" (T.s_of_loc node.T.n_loc)))
  | LocalSP off -> RMem (rSP, resolve_local_sp_offset env off)
  | LocalSPScaled (off, idx, scale) -> RMemIndexed (rSP, resolve_local_sp_offset env off, idx, scale)

(* claude: builds the ModRM(+SIB+disp) bytes for a `resolved_gen` used
 * as the r/m operand, with `reg_field` as ModRM.reg (either a real
 * register's own encoding, for register-to-register forms, or a
 * literal opcode-extension digit 0-7, for the immediate-group
 * instructions like 0x83/0xc7) -- ported from span.c's asmandsz(),
 * scoped down to just the addressing shapes hello_linux_amd64.s uses
 * (see this file's own prelude for what's not handled). Does not
 * include the REX byte (rexw is always emitted separately by each
 * caller below, unconditionally -- see this file's own rexw comment)
 * or the leading opcode byte. *)
let encode_rm (reg_field : int) (rm : resolved_gen) : int list =
  match rm with
  | RReg r -> [ modrm ~md:3 ~reg:reg_field ~rm:(reg_num r) ]
  | RMem (r, off) when reg_num r = 4 (* SP: rm=100 always needs a SIB
                                       * byte to follow, mod=11 aside --
                                       * real x86 ModRM quirk, not
                                       * SP-specific per se (R12 shares
                                       * it, not wired -- see prelude) *)
    ->
      let sib_byte = sib ~scale:0 ~index:4 (* no index *) ~base:4 (* SP *) in
      if off = 0
      then [ modrm ~md:0 ~reg:reg_field ~rm:4; sib_byte ]
      else if off >= -128 && off < 128
      then [ modrm ~md:1 ~reg:reg_field ~rm:4; sib_byte; off land 0xff ]
      else [ modrm ~md:2 ~reg:reg_field ~rm:4; sib_byte ] @ le32 off
  (* claude: any *ordinary* register as a plain memory base (no SIB
   * needed at all) -- confirmed against real 6a/6l: "CMPXCHGL
   * CX,0(BX)" -> `0f b1 0b` (ModRM alone, mod=0,rm=3(BX), no SIB
   * byte). BP/R13 (rm=101 mod=00 means RIP-relative in real amd64,
   * not "no displacement") and R12 (rm=100, same SIB-needed quirk as
   * SP) both need their own special cases this port doesn't have yet
   * -- see prelude -- so they're excluded here and still raise `Todo`
   * below; every *other* register (including R8-R11/R14-R15) needs
   * nothing beyond the ordinary mod/disp encoding every other arch's
   * own indirect addressing already uses. *)
  | RMem (r, off) when reg_num r land 7 <> 4 && reg_num r land 7 <> 5 ->
      let rmv = reg_num r land 7 in
      if off = 0
      then [ modrm ~md:0 ~reg:reg_field ~rm:rmv ]
      else if off >= -128 && off < 128
      then [ modrm ~md:1 ~reg:reg_field ~rm:rmv; off land 0xff ]
      else [ modrm ~md:2 ~reg:reg_field ~rm:rmv ] @ le32 off
  (* claude: BP/R13 as a memory base -- unlike every other register,
   * mod=00/rm=101 is reserved for RIP-relative addressing in real
   * amd64, not "no displacement", so an *actual* zero offset still
   * needs an explicit 1-byte $0 displacement (mod=01) -- confirmed
   * against real 6a/6l: "MOVBLZX (BP),AX" -> `0f b6 45 00` (mod=01,
   * disp8=0x00), not the disp-less `0f b6 45` a naive mod=00 read
   * would produce. Found stress-testing real lib_core/libc -- goken's
   * own compiled fmt.c/dofmt.c genuinely reuse BP as a plain struct-
   * pointer GPR (not a frame pointer) whenever a function doesn't
   * need one, e.g. "MOVQ 24(BP),SI". R12 (rm=100, the same mandatory-
   * SIB quirk as SP) isn't wired -- no real closure needs it yet. *)
  | RMem (r, off) when reg_num r land 7 = 5 ->
      let rmv = reg_num r land 7 in
      if off = 0
      then [ modrm ~md:1 ~reg:reg_field ~rm:rmv; 0 ]
      else if off >= -128 && off < 128
      then [ modrm ~md:1 ~reg:reg_field ~rm:rmv; off land 0xff ]
      else [ modrm ~md:2 ~reg:reg_field ~rm:rmv ] @ le32 off
  | RMem (_, _) ->
      raise Todo (* R12 (mandatory SIB, like SP) as a memory base isn't
                   * wired, see prelude *)
  | RAbs addr ->
      (* claude: mod=00/rm=100(SIB)/SIB=no-index,no-base(base=101) ->
       * absolute disp32 -- goken's own non-PIE amd64 addressing
       * convention for D_EXTERN/D_STATIC symbols (span.c's asmandsz()
       * "temporary" comment: real bytes confirmed for "LEAQ msg(SB),AX"
       * -> "48 8d 04 25 <disp32>"). *)
      [ modrm ~md:0 ~reg:reg_field ~rm:4; sib ~scale:0 ~index:4 ~base:5 ] @ le32 addr

  (* claude: SIB scaled-index case (`gen`'s IndirectScaled) -- same
   * base-register split as plain `RMem` just above (SP always needs
   * a SIB anyway, so it needs no separate case here at all -- unlike
   * plain `RMem`, where SIB is *only* needed because SP happens to
   * share rm=100 with "SIB follows"; here a SIB is already mandatory
   * for the index, so SP-as-base falls out of the ordinary case for
   * free). BP/R13 excluded for the same rip-relative-mod=00 reason as
   * plain `RMem`'s own BP/R13 exclusion -- not wired, see prelude. *)
  | RMemIndexed (r, off, idx, scale) when reg_num r land 7 <> 5 ->
      let scale_bits = match scale with 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3
        | _ -> raise (Impossible "scale must be 1/2/4/8 -- checked by the grammar") in
      let sib_byte = sib ~scale:scale_bits ~index:(reg_num idx land 7) ~base:(reg_num r land 7) in
      if off = 0
      then [ modrm ~md:0 ~reg:reg_field ~rm:4; sib_byte ]
      else if off >= -128 && off < 128
      then [ modrm ~md:1 ~reg:reg_field ~rm:4; sib_byte; off land 0xff ]
      else [ modrm ~md:2 ~reg:reg_field ~rm:4; sib_byte ] @ le32 off
  | RMemIndexed (_, _, _, _) ->
      raise Todo (* BP/R13 as a scaled-index base -- not wired, see prelude *)

  (* claude: SIB scaled-index off an SB-relative global (`gen`'s
   * EntityScaled) -- same no-base disp32 convention as plain `RAbs`
   * above, just with a real index register in the SIB instead of
   * SIB's own "no index" encoding. *)
  | RAbsIndexed (addr, idx, scale) ->
      let scale_bits = match scale with 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3
        | _ -> raise (Impossible "scale must be 1/2/4/8 -- checked by the grammar") in
      [ modrm ~md:0 ~reg:reg_field ~rm:4; sib ~scale:scale_bits ~index:(reg_num idx land 7) ~base:5 ] @ le32 addr

let arith_ext = function ADD -> 0 | SUB -> 5 | XOR -> 6 | AND -> 4 | OR -> 1
(* claude: the reg-reg ("Zr_m") opcode -- confirmed against goken's own
 * optab.c that B_'s own opcode is always exactly one less than L_/Q_/
 * W_'s shared one (ADD 0x00 vs 0x01, SUB 0x28 vs 0x29, XOR 0x30 vs
 * 0x31, AND 0x20 vs 0x21, OR 0x08 vs 0x09), matching real x86's own
 * "even opcode = 8-bit form" encoding convention -- but kept as an
 * explicit table, not opcode-1 arithmetic, so a future opcode this
 * pattern doesn't hold for can't silently rely on it. *)
let arith_rr_opcode (width : width) (op : arith_opcode) : int =
  match width, op with
  | B_, ADD -> 0x00 | B_, SUB -> 0x28 | B_, XOR -> 0x30 | B_, AND -> 0x20 | B_, OR -> 0x08
  | (Q_ | L_ | W_), ADD -> 0x01 | (Q_ | L_ | W_), SUB -> 0x29 | (Q_ | L_ | W_), XOR -> 0x31
  | (Q_ | L_ | W_), AND -> 0x21 | (Q_ | L_ | W_), OR -> 0x09

(* claude: the immediate-group opcode (ModRM-extension-dispatched, used
 * by both Arith's and Cmp's own immediate forms) -- 0x80 for a true
 * 8-bit destination (the immediate byte is the *whole* value, not a
 * sign-extended-into-a-wider-register special case), 0x83 for Q_/L_/W_
 * (goken's own Yi8 sign-extends into the wider destination -- see
 * Arith's own imm32-not-wired comment for the *general*-immediate 0x81
 * form this doesn't cover). *)
let imm_group_opcode (width : width) : int = match width with B_ -> 0x80 | Q_ | L_ | W_ -> 0x83

(* claude: the "wide" (non-imm8) immediate range/encoding for Arith's
 * own Zil_/Zilo_m rows (opcode 0x81/AX-implicit) -- W_'s own immediate
 * is 2 bytes (confirmed: "ANDW $0x1234,AX" -> `66 25 34 12`), Q_/L_'s
 * is 4 (sign-extended to 64 for Q_, same as every other wide-immediate
 * form in this file). B_ never reaches either case (its own 0x80 form
 * has no imm8-vs-wider split at all, see `imm_group_opcode`'s own
 * comment), so both helpers are partial over width by design. *)
(* claude: goken's own `oclass()` (span.c) classifies an immediate
 * purely by its own value, regardless of the destination instruction's
 * width: `Ys32` (sign-extendable 32-bit, -0x8000_0000..0x7fff_ffff) is
 * a *subset* of the wider `Yi32` ("fits in 32 bits, zero-extended",
 * i.e. `(v>>32)==0`) -- a value like 0xFFFFFFF6, written as a positive
 * hex literal, lands in `Yi32` but *not* `Ys32` (confirmed reading
 * `oclass()`: `l = v; if((vlong)l==v) return Ys32; if((v>>32)==0)
 * return Yi32;`, `l` a 32-bit local). `Move`'s own immediate clauses
 * need this exact range (not just `Ys32`'s narrower one) for Q_/L_/W_
 * alike -- confirmed against real 6a/6l this port's own earlier,
 * narrower guard was a real gap (see amd64_port.md's own "Real
 * bugs/quirks"): "MOVQ $0xFFFFFFF6,AX" -> `b8 f6 ff ff ff` (`Ziq_rp`'s
 * own internal no-REX.W downgrade, see `move_q_wide_reg_opcode`),
 * "MOVL $0xFFFFFFF6,BX" -> `bb f6 ff ff ff` (same opcode family,
 * always, no split at all for L_), "MOVW $0x12345678,CX" ->
 * `66 b9 78 56` (even a value exceeding 32 bits' own relevant range
 * is accepted and simply truncated to the low 16 bits -- `wide_imm_bytes`'s
 * own `land`-based truncation already produces the correct bytes for
 * any of these regardless of magnitude, so only the *range guard*
 * needed widening, no encoding logic). *)
let fits_yi32 (v : int) : bool = v >= -0x8000_0000 && v <= 0xFFFF_FFFF

(* claude: L_/W_ get the same `oclass()`-style widened range as
 * `fits_yi32` above (accepting a value like 0xFFFFFFFB, written as a
 * positive decimal, that doesn't fit the narrower sign-extendable
 * `Ys32`/`Ys16` range) -- confirmed against real 6a/6l stress-testing
 * lib_core/libc's own "ANDL $4294967291,76(BP)" (port/frexp.c's real
 * clear-the-sign-bit idiom). Safe *unlike* Move's own Q_ case (which
 * needs `move_q_wide_reg_opcode`'s own no-REX.W downgrade to avoid
 * silently sign-extending a value the source never meant as negative)
 * because L_/W_'s own destination IS exactly the immediate's own
 * width already -- opcode 0x81 never sign-extends an Yi32 into a wider
 * L_/W_ register the way it does for Q_'s genuine 32-into-64 REX.W
 * extension, so accepting the raw zero-extended pattern changes
 * nothing about the emitted bytes' *meaning*, just widens which values
 * this port accepts. Q_ stays at the narrower true-Ys32 range -- no
 * real closure stress-tested so far needs a wider Q_ immediate here,
 * and doing so safely would need the same kind of opcode-downgrade
 * `move_q_wide_reg_opcode` uses, not just a wider range check. *)
let fits_wide_imm (width : width) (v : int) : bool =
  match width with
  | W_ -> (v >= -0x8000 && v <= 0x7fff) || (v >= 0 && v <= 0xffff)
  | L_ -> (v >= -0x8000_0000 && v <= 0x7fff_ffff) || (v >= 0 && v <= 0xffff_ffff)
  | Q_ -> v >= -0x8000_0000 && v <= 0x7fff_ffff
  | B_ -> raise (Impossible "B_ has no wide-immediate arith form")
let wide_imm_bytes (width : width) (v : int) : int list =
  match width with
  | W_ -> le16 v
  | Q_ | L_ -> le32 v
  | B_ -> raise (Impossible "B_ has no wide-immediate arith form")

(* claude: Cmp's own "gen,reg" (Zm_r) direction opcode -- same
 * B_-is-one-less pattern as `arith_rr_opcode` (CMP r/m8,r8 = 0x38 vs
 * r/m32,r32 = 0x39), confirmed against optab.c's ycmpb/ycmpl. *)
let cmp_rm_opcode (width : width) : int = match width with B_ -> 0x38 | Q_ | L_ | W_ -> 0x39

(* claude: goken's own ytestl/ytestb tables -- same B_-is-one-less
 * opcode pair, confirmed against real 6a/6l: "TESTL BX,CX" -> `85 d9`,
 * "TESTB DX,DX" -> `84 d2`. *)
let test_opcode (width : width) : int = match width with B_ -> 0x84 | Q_ | L_ | W_ -> 0x85

(* claude: goken's own yrl_ml/yrb_mb tables -- same B_-is-one-less
 * opcode pair, both `0x0f`-escaped. Confirmed against real 6a/6l:
 * "CMPXCHGL CX,0(BX)" -> `0f b1 0b`, "CMPXCHGB DX,0(BX)" -> `0f b0 13`. *)
let cmpxchg_opcode (width : width) : int = match width with B_ -> 0xb0 | Q_ | L_ | W_ -> 0xb1

(* claude: Move's own store (Zr_m) / load (Zm_r) opcodes -- same
 * B_-is-one-less pattern yet again (MOV r/m8,r8 = 0x88 vs r/m32,r32 =
 * 0x89; MOV r8,r/m8 = 0x8a vs r32,r/m32 = 0x8b), confirmed against
 * optab.c's ymovb/ymovl. *)
let mov_store_opcode (width : width) : int = match width with B_ -> 0x88 | Q_ | L_ | W_ -> 0x89
let mov_load_opcode (width : width) : int = match width with B_ -> 0x8a | Q_ | L_ | W_ -> 0x8b

let shift_ext = function SHL -> 4 | SHR -> 5 | SAR -> 7
(* claude: goken's own `yshl`/`yshb` tables (optab.c) have three shift-
 * amount shapes, all three sharing the exact same B_-is-one-less
 * pattern as every other arith-family opcode in this file: shift-by-1
 * (goken's `Yi1` class -- opcode alone, no immediate byte at all, only
 * reached when the immediate is *literally* 1, see Ast_asm6.ml's
 * `Shift`/`shift_amount` comment), shift-by-immediate-N (`Yi32`,
 * opcode+ModRM+1-byte immediate), and shift-by-CL/CX (`Ycl`/`Ycx`,
 * opcode+ModRM, amount implicit). Confirmed against real 6a/6l: "SHLQ
 * $1,AX" -> `48 d1 e0`, "SHLQ $4,AX" -> `48 c1 e0 04`, "SHLQ CX,AX" ->
 * `48 d3 e0`; "SHRB $4,DX" -> `c0 ea 04` (0xc0, one less than L/Q/W's
 * 0xc1); "SHLB CX,DX" -> `d2 e2` (0xd2, one less than 0xd3). *)
let shift_by1_opcode (width : width) : int = match width with B_ -> 0xd0 | Q_ | L_ | W_ -> 0xd1
let shift_byimm_opcode (width : width) : int = match width with B_ -> 0xc0 | Q_ | L_ | W_ -> 0xc1
let shift_bycl_opcode (width : width) : int = match width with B_ -> 0xd2 | Q_ | L_ | W_ -> 0xd3

(* claude: `Extend`'s own per-mnemonic shape -- `need_w` (REX.W, the
 * "Q"-suffixed forms), `byte_source` (whether the *source* needs the
 * byte-register REX-forcing quirk `regrex_forces_rex` already
 * describes -- goken's own Zmb_r case calls `bytereg()` on the source
 * for the byte-sized forms only, confirmed against real 6a/6l:
 * "MOVBLZX SI,DX" -> `40 0f b6 d6`, forcing an otherwise-empty REX,
 * vs "MOVWLZX AX,BX" -> `0f bf d8`, no such forcing for a word
 * source), and the opcode bytes themselves (always `0x0f`-escaped
 * except MOVLQSX's own real MOVSXD, `0x63`, no escape at all). *)
let extend_shape = function
  | MOVBLSX -> (false, true, [0x0f; 0xbe]) | MOVBLZX -> (false, true, [0x0f; 0xb6])
  | MOVBQSX -> (true, true, [0x0f; 0xbe]) | MOVBQZX -> (true, true, [0x0f; 0xb6])
  | MOVWLSX -> (false, false, [0x0f; 0xbf]) | MOVWLZX -> (false, false, [0x0f; 0xb7])
  | MOVWQSX -> (true, false, [0x0f; 0xbf]) | MOVWQZX -> (true, false, [0x0f; 0xb7])
  | MOVLQSX -> (true, false, [0x63])
  | MOVLQZX -> (false, false, [0x8b])

(* claude: a bespoke REX computation for `Extend`, not `rex_opt` --
 * `rex_opt`'s own byte-register-forcing logic is gated on `width =
 * B_`, which doesn't apply here (there's no destination-width
 * variance in the traditional Q_/L_/W_/B_ sense: each `extend_opcode`
 * already fully determines both the source and destination width by
 * its own name), so the REX.W and byte-source-forcing concerns are
 * threaded independently instead. *)
let extend_rex ~(need_w : bool) ~(byte_source : bool) ~(reg_field : int)
    ~(rm : resolved_gen) : int list =
  let w = if need_w then 8 else 0 in
  let r = if reg_field >= 8 then 4 else 0 in
  let x = rex_x_of_resolved_gen rm lsl 1 in
  let b = rex_b_of_resolved_gen rm in
  let forced =
    byte_source &&
    (match rm with
     | RReg (A.R n) -> regrex_forces_rex n
     | RMem _ | RAbs _ | RMemIndexed _ | RAbsIndexed _ -> false)
  in
  if w <> 0 || r <> 0 || x <> 0 || b <> 0 || forced then [ 0x40 lor w lor r lor x lor b ] else []

(* claude: goken's own yincb/yincl/yincw/yscond tables (optab.c) --
 * NEG/NOT share the same B_-is-one-less opcode pair as every other
 * arith-family instruction in this file (0xf6 vs 0xf7); INC/DEC
 * likewise (0xfe vs 0xff). Ext digits: NEG=3, NOT=2 (goken's own
 * `yscond` rows), INC=0, DEC=1 (goken's own `yincb`/`yincl`/`yincw`
 * rows) -- confirmed against real 6a/6l throughout. *)
let unary_ext = function NEG -> 3 | NOT -> 2 | INC -> 0 | DEC -> 1
let unary_opcode (width : width) (op : unary_opcode) : int =
  match width, op with
  | B_, (NEG | NOT) -> 0xf6 | B_, (INC | DEC) -> 0xfe
  | (Q_ | L_ | W_), (NEG | NOT) -> 0xf7 | (Q_ | L_ | W_), (INC | DEC) -> 0xff

(* claude: goken's own ydivl/ydivb tables (optab.c) -- same B_-is-one-
 * less opcode pair as `Unary`'s own NEG/NOT (0xf6 vs 0xf7), confirmed
 * against real 6a/6l: "IDIVQ BX" -> `48 f7 fb`, "DIVL BX" -> `f7 f3`. *)
let muldiv_ext = function MUL_ -> 4 | IMUL_ -> 5 | DIV_ -> 6 | IDIV_ -> 7
let muldiv_opcode (width : width) : int = match width with B_ -> 0xf6 | Q_ | L_ | W_ -> 0xf7

(* claude: an `xgen` (XMM register-or-memory operand) coerced into the
 * *existing* `gen` type before resolution -- goken's own D_X0..D_X0+15
 * REX/ModRM encoding is numerically identical to the GP register
 * scheme this whole file already implements (see Ast_asm6.ml's
 * `xregister` comment: X8-X15 need REX.R/.B exactly like R8-R15), so
 * rather than duplicating `resolve_gen`/`resolve_gen_full`/`encode_rm`/
 * `rex_opt` for a second register file, an `xgen` is coerced into an
 * "as if GP register" `gen` here and threaded through those *unchanged*
 * -- safe because none of them ever inspect *which* register file a
 * `resolved_gen`'s number came from, only the raw 0-15 value (and
 * `width` is never `B_` for any float instruction below, so the B_-only
 * AH/BH-quirk logic in `rex_opt` never triggers here either). *)
let gen_of_xgen : xgen -> gen = function
  | XReg (X n) -> GReg (A.R n)
  | XIndirect (r, off) -> Indirect (r, off)
  | XEntity e -> Entity e
  | XIndirectScaled (r, off, idx, scale) -> IndirectScaled (r, off, idx, scale)
  | XEntityScaled (e, idx, scale) -> EntityScaled (e, idx, scale)
  | XLocalSP off -> LocalSP off
  | XLocalSPScaled (off, idx, scale) -> LocalSPScaled (off, idx, scale)
  | XFloatImm _ ->
      raise (Impossible "XFloatImm must be eliminated by Rewrite6.rewrite \
                          before Codegen6.gen ever runs -- see Ast_asm6.ml's own comment")

(* claude: every call site below resolves a `gen_of_xgen` result with
 * `resolve_gen_full`, not `resolve_gen` -- an XMM instruction's memory
 * operand can equally well name a SData2 (DATA-segment) global (real
 * fmt/fltfmt.c's own "MOVSD pows10<>+0(SB)(AX*8),X0", a table of double
 * constants), which `resolve_gen`'s own narrower version can't resolve
 * (see its own SData2 comment). *)

(* claude: goken's own `yxm` table (optab.c) is shared verbatim across
 * ADDSD/SUBSD/MULSD/DIVSD (and their SS-suffixed siblings, see
 * `sse_prefix` below) -- only this final opcode byte differs, and only
 * per *operation*, not per precision. *)
let arithf_opcode_byte = function FADD -> 0x58 | FSUB -> 0x5c | FMUL -> 0x59 | FDIV -> 0x5e

(* claude: the `Pf2`/`Pf3` legacy prefix MOVSD/MOVSS, ADDSD/ADDSS (and
 * every other `yxm`/`yxmov`/`yxcvlf`/`yxcvfq`-shaped instruction below
 * except UCOMISD/UCOMISS, see `ucomis_prefix`) share -- 0xf2 for
 * double precision, 0xf3 for single, confirmed against real 6a/6l
 * throughout (e.g. "ADDSS X1,X0" -> `f3 0f 58 c1`, same opcode byte as
 * ADDSD's own `f2 0f 58 c1`, just the prefix swapped). *)
let sse_prefix : A.floatp_precision -> int = function A.D -> 0xf2 | A.F -> 0xf3

(* claude: UCOMISD/UCOMISS's own prefix story is a genuine third case,
 * different from every other SSE instruction here -- UCOMISD is `Pe`
 * (0x66, confirmed "UCOMISD X1,X0" -> `66 0f 2e c1`), UCOMISS is `Pm`,
 * i.e. *no* legacy prefix byte at all (confirmed "UCOMISS X1,X0" ->
 * `0f 2e c1`, straight to the 0x0f escape). *)
let ucomis_prefix : A.floatp_precision -> int list = function A.D -> [0x66] | A.F -> []

(* claude: the *short* (rel8) Jcc opcode for each condition -- goken's
 * optab.c AJEQ/AJNE/.../AJLS entries each list {short_op, near_op}
 * (e.g. AJEQ: 0x74,0x84); only the short one is used here, see
 * Jcc/Jmp's own comment below for why. *)
let jcc_short_opcode = function
  | EQ -> 0x74 | NE -> 0x75
  | LT A.S -> 0x7c | GE A.S -> 0x7d | GT A.S -> 0x7f | LE A.S -> 0x7e
  | LT A.U -> 0x72 | GE A.U -> 0x73 | GT A.U -> 0x77 | LE A.U -> 0x76

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* claude: init_data mirrors Codegen.default_rules's own second
 * parameter -- needed to turn a SData2 (data-segment) offset into an
 * absolute address, unlike a SText2 (procedure) one, which is already
 * absolute (real_pc). Layout6.ml's sizing-only pass calls `rules` with
 * init_data always None (the whole text segment's size -- and hence
 * where the data segment starts -- isn't known until *after* that pass
 * finishes), which would make any "LEAQ dataglobal(SB),R" crash before
 * ever reaching Codegen6.gen's own real pass, where init_data is always
 * Some -- confirmed the hard way: msg is a SData2 global, and this
 * genuinely needs to work for the sizing pass too, not just the final
 * one. The resolved address only ever affects `encode_rm`'s RAbs
 * disp32 *value*, never the byte *count* it produces, so any
 * placeholder value is safe to size with -- 0 is used here. *)
let resolve_global_addr (env : Codegen.env) (init_data : T.addr option)
    (glob : A.global) (off : A.offset) : int =
  match Hashtbl.find env.syms (T.symbol_of_global glob) with
  | T.SText2 real_pc -> real_pc + off
  | T.SData2 (data_off, _kind) ->
      (match init_data with
      | None -> 0 (* sizing pass only -- see comment above *)
      | Some init_data -> init_data + data_off + off
      )

let resolve_gen_full (env : Codegen.env) (init_data : T.addr option)
    (node : 'a T.node) (g : gen) : resolved_gen =
  match g with
  | Entity (A.Global (glob, off)) -> RAbs (resolve_global_addr env init_data glob off)
  (* claude: EntityScaled's own SData2-capable sibling -- e.g. real
   * port_ctype.c's own "_ctype+0(SB)(CX*1)" (a DATA-segment table, not
   * a TEXT symbol), which `resolve_gen`'s own EntityScaled case can't
   * handle (see `resolve_global_text_addr`'s SText2-only comment). *)
  | EntityScaled (A.Global (glob, off), idx, scale) ->
      RAbsIndexed (resolve_global_addr env init_data glob off, idx, scale)
  | _ -> resolve_gen env node g

let arith_rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node)
    (instr : instr) : action =
  match instr with
    (* claude: case Zib_ -- goken's own yxorb table has its
     * "Yi32,Yal,Zib_,1" row (opcode alone + imm8, no ModRM at all --
     * real x86's dedicated
     * "op AL,imm8" encoding) listed *before* the general
     * "Yi32,Ymb,Zibo_m,2" ModRM row, so a destination of exactly AX
     * (goken's Yal, i.e. register index 0 -- *not* R8, whose low 3
     * bits also happen to be 0 but which has no such AL-only encoding
     * at all) always takes this shorter form, confirmed against real
     * 6a/6l: "ADDB $3,AX"/"SUBB $1,AX"/"XORB $0xff,AX" -> "04 03"/
     * "2c 01"/"34 ff" (2 bytes each, no REX, no ModRM) -- unlike
     * "ADDB $3,BX" -> "80 c3 03" (general ModRM form). This is the
     * *opposite* row order from ycmpl/yxorl's own Yax-vs-Yml special
     * case (see the Cmp clauses below and this file's own B_ comments)
     * -- there, the general ModRM row comes *first* and so always wins
     * for any immediate that fits imm8, meaning Q_/L_/W_ never need
     * this special case at all; B_ is the only width where it's
     * actually reachable. Opcode pattern: (ext<<3)|0x04, matching
     * every other arch-independent "reg,imm8-group" opcode-embedding
     * convention already seen elsewhere in this file. *)
    | Arith (B_, op, Imm v, GReg r) when reg_num r = 0 && v >= -128 && v <= 255 ->
        let bytes = [(arith_ext op lsl 3) lor 0x04; v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Arith (B_, op, Imm v, dest) when v >= -128 && v <= 255 ->
        (* claude: case Zibo_m -- B_'s own 0x80 immediate-group form has no "does it
         * fit imm8" question at all -- the immediate byte *is* the
         * whole destination width, so (unlike Q_/L_/W_'s Yi8-vs-Yi32
         * split below) there's no larger form to fall back to; the
         * full unsigned byte range (not just -128..127) is accepted. *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = rex_opt ~reg_is_register:false ~width:B_ ~reg_field:(arith_ext op) ~rm
                    @ [imm_group_opcode B_] @ encode_rm (arith_ext op) rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zibo_m -- goken's yxorl/yaddl's own Yi8 row, same
     * shape as B_'s own case above (which never reaches here, see
     * that clause), just L_/Q_/W_'s wider sign-extended-imm8 form. *)
    | Arith (width, op, Imm v, dest) when v >= -128 && v < 128 ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(arith_ext op) ~rm
                    @ [imm_group_opcode width] @ encode_rm (arith_ext op) rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zil_ -- goken's own "Yi32,Yax,Zil_,1" row, the
     * Q_/L_/W_-width mirror of B_'s own AL-special-case above: a
     * destination of exactly AX (never RAX/EAX/AX generically -- Yax
     * is register index 0 specifically) takes an opcode-alone,
     * no-ModRM form ahead of the general Zilo_m row below, whenever
     * the immediate doesn't already fit the narrower imm8 row above.
     * Same `(ext<<3)|k` opcode-embedding family as the B_-width case,
     * just `k=0x05` instead of `0x04` (confirmed against real 6a/6l:
     * "ANDL $0xFF,AX" -> `25 ff 00 00 00`, "ADDQ $1000,AX" ->
     * `48 05 e8 03 00 00`, "XORQ $1000,AX" -> `48 35 e8 03 00 00` --
     * matching real x86's own ADD/OR/AND/SUB/XOR/CMP "op AX,imm"
     * opcode family, `(ext<<3)|0x05`). W_'s own immediate is 2 bytes,
     * not 4 (confirmed: "ANDW $0x1234,AX" -> `66 25 34 12`). REX.W
     * still needs computing for Q_ even though there's no ModRM byte
     * here -- `rm:(RReg r)` (AX, index 0) never itself contributes,
     * but `rex_opt`'s own `width` parameter does. *)
    | Arith (width, op, Imm v, GReg r) when reg_num r = 0 && fits_wide_imm width v ->
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:0 ~rm:(RReg r)
                    @ [(arith_ext op lsl 3) lor 0x05] @ wide_imm_bytes width v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zilo_m -- the general ModRM form (opcode 0x81),
     * reached whenever the destination isn't AX or the immediate
     * doesn't fit imm8. Same imm16-for-W_/imm32-for-Q_/L_ split as the
     * Zil_ case above. *)
    | Arith (width, op, Imm v, dest) when fits_wide_imm width v ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(arith_ext op) ~rm
                    @ [0x81] @ encode_rm (arith_ext op) rm @ wide_imm_bytes width v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Arith (_, _op, Imm _, _dest) ->
        raise Todo (* immediate too big even for Zilo_m's own imm32/imm16 -- see prelude *)
    (* claude: case Zr_m -- goken's yxorl/yxorb's own Yrl/Yrb,Yml/Ymb
     * row (register source, `encode_rm`'s own "reg-reg or reg-mem"
     * dest). *)
    | Arith (width, op, Reg r, dest) ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [arith_rr_opcode width op] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r -- "op mem,reg" (e.g. real "SUBQ s+0(FP),AX"),
     * goken's yaddl own Yml,Yrl row -- same reg-field/rm split as
     * Move's own Zm_r load (mov_load_opcode), just arith_rr_opcode+2
     * (confirmed against 6l/optab.c: every yaddl row's Zm_r opcode is
     * its own Zr_m opcode plus 2, e.g. ADD 0x01->0x03, SUB 0x29->0x2b,
     * same for the B_-width 0x00->0x02/0x28->0x2a pairs). Only a
     * register destination is reached here -- goken's own row has no
     * mem-to-mem form, and the grammar never produces `Mem (GReg _)`
     * (see Ast_asm6.ml's `imr` comment), so a `Mem` source with a
     * non-register dest can't arise from real input. *)
    | Arith (width, op, Mem src, GReg r) ->
        let rm = resolve_gen_full env init_data node src in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [arith_rr_opcode width op + 2] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Arith (_, _, Mem _, (Indirect _ | Entity _ | IndirectScaled _ | EntityScaled _)) ->
        raise Todo (* goken's own yaddl has no mem-to-mem Zm_r row *)
    | Arith (_, _, Addr _, _) ->
        raise Todo (* an address-of-global immediate as an Arith source
                     * (see Ast_asm6.ml's `imr`/Addr comment) -- not
                     * wired, no real closure needs ADD/SUB/etc. against
                     * one yet, only CMP's own Zm_ilo case does *)

    (* claude: case Zm_ibo -- "CMPQ gen,$imm", opcode 0x83 /7, `gen` in
     * ModRM r/m (see Ast_asm6.ml's Cmp comment: the ModRM operand is
     * `gen` here even though it's the *first* operand, unlike Arith's
     * own Zibo_m where `gen` -- the *second* operand -- plays that
     * role). Only the imm8 form is wired, same scope restriction as
     * Arith's own immediate case. *)
    (* claude: case Z_ib -- ycmpb's own "Yal,Yi32,Z_ib,1" row -- goken's
     * byte-width mirror of Arith's own AL-special-case above (same
     * reg-index-0-only, opcode-alone-no-ModRM shape, opcode 0x3c).
     * Confirmed against real 6a/6l: "CMPB AX,$5" -> "3c 05". Placed
     * before the general B_ clause below for the same reason. *)
    | Cmp (B_, GReg r, Imm v) when reg_num r = 0 && v >= -128 && v <= 255 ->
        let bytes = [0x3c; v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zm_ibo (B_) -- same "no imm8-vs-imm32 split, the
     * byte IS the whole value" reasoning as Arith's own B_ clause
     * above. *)
    | Cmp (B_, g, Imm v) when v >= -128 && v <= 255 ->
        let rm = resolve_gen_full env init_data node g in
        let bytes = rex_opt ~reg_is_register:false ~width:B_ ~reg_field:7 ~rm
                    @ [imm_group_opcode B_] @ encode_rm 7 rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zm_ibo (Q_/L_/W_). *)
    | Cmp (width, g, Imm v) when v >= -128 && v < 128 ->
        let rm = resolve_gen_full env init_data node g in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:7 ~rm
                    @ [imm_group_opcode width] @ encode_rm 7 rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Z_il -- ycmpl's own "Yax,Yi32,Z_il,1" row, Cmp's own
     * mirror of Arith's Zil_ case above -- opcode 0x3d (matching the
     * `(ext<<3)|0x05` family, ext=7 for CMP), reached whenever `g` is
     * exactly AX and the immediate doesn't fit imm8. Confirmed against
     * real 6a/6l: "CMPQ AX,$-1000" -> `48 3d 18 fc ff ff`. *)
    | Cmp (width, GReg r, Imm v) when reg_num r = 0 && fits_wide_imm width v ->
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:0 ~rm:(RReg r)
                    @ [(7 lsl 3) lor 0x05] @ wide_imm_bytes width v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zm_ilo -- the general ModRM form (opcode 0x81 /7),
     * reached whenever `g` isn't AX or the immediate doesn't fit imm8.
     * Confirmed: "CMPL CX,$0x100000" -> `81 f9 00 00 10 00`. *)
    | Cmp (width, g, Imm v) when fits_wide_imm width v ->
        let rm = resolve_gen_full env init_data node g in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:7 ~rm
                    @ [0x81] @ encode_rm 7 rm @ wide_imm_bytes width v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Cmp (_, _g, Imm _) ->
        raise Todo (* immediate too big even for Zm_ilo's own imm32/imm16 -- see prelude *)
    (* claude: case Zm_ilo -- same general ModRM form as the `Imm` clause
     * just above, just with an address-of-global immediate (see
     * Ast_asm6.ml's `imr`/Addr comment) instead of a literal constant --
     * real fmt/fmt.c's own "CMPQ DX,$fmtalloc<>+1032(SB)" (comparing a
     * moving pointer against the end of a static allocation pool). The
     * address itself is resolved lazily in `binary`'s own thunk, same
     * forward-reference-safety reasoning as `Lea`'s own Global case --
     * `fmtalloc<>` happens to be a DATA (SData2) global here, not a
     * forward-referenced TEXT one, but there's no way to tell which
     * case a given global is without the same lookup that would fail
     * for a forward TEXT reference, so this is unconditionally
     * deferred too, same as `Lea`. A Local/Param address (not
     * meaningful for CMP) isn't wired -- no real closure needs it. *)
    (* claude: case Z_il -- same AX-special-case as the `Imm` clause
     * above (opcode 0x3d, the `(7<<3)|0x05` family), just with a
     * lazily-resolved address value instead of a literal -- confirmed
     * against real 6a/6l: "CMPQ AX,$tab<>+8(SB)" -> "48 3d <imm32>",
     * not the general Zm_ilo ModRM form the clause below falls back
     * to for any other destination. Always the imm32 form (never the
     * imm8 one `Imm`'s own Z_il case can also take) -- same "every
     * address here fits 32 bits by construction" reasoning as MOVQ's
     * own address-immediate case above. *)
    | Cmp (width, GReg r, Addr (A.Global (glob, off))) when reg_num r = 0 ->
        let bytes_placeholder =
          prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:0 ~rm:(RReg r)
          @ [(7 lsl 3) lor 0x05] @ wide_imm_bytes width 0 in
        { size = List.length bytes_placeholder; binary = (fun () ->
            let addr = resolve_global_addr env init_data glob off in
            prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:0 ~rm:(RReg r)
            @ [(7 lsl 3) lor 0x05] @ wide_imm_bytes width addr
          )
        }
    | Cmp (width, g, Addr (A.Global (glob, off))) ->
        let rm = resolve_gen_full env init_data node g in
        let rm_placeholder = rm in
        let bytes_placeholder =
          prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:7 ~rm:rm_placeholder
          @ [0x81] @ encode_rm 7 rm_placeholder @ wide_imm_bytes width 0 in
        { size = List.length bytes_placeholder; binary = (fun () ->
            let addr = resolve_global_addr env init_data glob off in
            prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:7 ~rm
            @ [0x81] @ encode_rm 7 rm @ wide_imm_bytes width addr
          )
        }
    | Cmp (_, _, Addr _) ->
        raise Todo (* a Local/Param address -- not meaningful for CMP,
                     * not wired, see the clause above *)
    (* claude: case Zr_m -- the *reverse*-direction "CMPQ Rs,mem" row
     * (goken's own ycmpl "Yrl,Yml,Zr_m,1" row, opcode 0x3b/0x3a --
     * `cmp_rm_opcode`'s own Zm_r opcode plus 2, same +2 convention as
     * Arith's own Zm_r->Zm_r+2 and Move's own store->load pair) --
     * genuinely the OPPOSITE role order from the `Cmp (width, g, Reg
     * r)` clause just below (there, `g` -- possibly memory -- is
     * ModRM.rm and `imr`'s register is ModRM.reg; here `g` is *always*
     * a plain register, in ModRM.reg, and `imr`'s own `Mem` is
     * ModRM.rm) -- both are real, distinct goken y-table rows, not one
     * generalizing the other. Confirmed against real fmt/dofmt.c's own
     * "CMPL DX,n+8(FP)"/"CMPQ AX,rs+-16(SP)" (comparing a register
     * against a named local). *)
    | Cmp (width, GReg r, Mem m) ->
        let rm = resolve_gen_full env init_data node m in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [cmp_rm_opcode width + 2] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Cmp (_, _g, Mem _) ->
        raise Todo (* "CMPQ mem,mem" -- not real x86, no real closure
                     * needs this combination *)
    (* claude: case Zm_r -- "CMPQ gen,Rs", opcode 0x39 (0x38 for B_, see
     * `cmp_rm_opcode`), `gen` in ModRM r/m, Rs in ModRM reg (asmand
     * (from=gen,to=Rs), matching Ast_asm6.ml's Cmp comment). The
     * reverse-direction row (case Zr_m, opcode 0x3b/0x3a, for when
     * `gen` should land in ModRM reg instead) is a separate goken
     * y-table row this port doesn't need yet -- not wired. *)
    | Cmp (width, g, Reg r) ->
        let rm = resolve_gen_full env init_data node g in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [cmp_rm_opcode width] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zr_m -- "TESTQ Rs,gen", opcode 0x85 (0x84 for B_),
     * reg_field=Rs (the *first* written operand -- a third role-order,
     * distinct from both Arith's and Cmp's own, see Ast_asm6.ml's Test
     * comment), rm=gen. *)
    | Test (width, r, g) ->
        let rm = resolve_gen_full env init_data node g in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [test_opcode width] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zr_m -- "CMPXCHGQ Rs,gen", opcode 0x0f 0xb1 (0x0f
     * 0xb0 for B_), same role order as Arith's own Reg-source case and
     * Move's own store clause (reg_field=Rs, rm=gen). *)
    | CmpXchg (width, r, dest) ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [0x0f; cmpxchg_opcode width] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zlit -- LOCK, a single fixed opcode byte (0xf0). *)
    | Lock -> { size = 1; binary = (fun () -> [0xf0]) }

    (* claude: case Zm_r_xm -- raw GP->XMM MOVQ (real x86's "MOVQ
     * xmm,r/m64", opcode `0x66 REX.W 0F 6E`) -- see Ast_asm6.ml's
     * MovQToXmm comment. Always REX.W (unlike every other `0x66`-
     * prefixed instruction in this file, this one's REX.W is
     * mandatory, not width-selected -- there's no 32-bit "MOVQ" to
     * confuse it with, since this whole shape is MOVQ-only). *)
    | MovQToXmm (src, dst) ->
        let rm = resolve_gen_full env init_data node src in
        let bytes = [0x66] @ rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(xreg_num dst) ~rm
                    @ [0x0f; 0x6e] @ encode_rm (xreg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zr_m_xm -- the reverse direction, "MOVQ r/m64,xmm",
     * opcode `0x66 REX.W 0F 7E`. *)
    | MovQFromXmm (src, dst) ->
        let rm = resolve_gen_full env init_data node dst in
        let bytes = [0x66] @ rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(xreg_num src) ~rm
                    @ [0x0f; 0x7e] @ encode_rm (xreg_num src) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zibo_m_xm -- PSLLQ's own shift-by-immediate form
     * (goken's `yps` table) -- opcode `0x66 0F 73 /6`, no REX at all
     * (SSE2 packed ops don't need REX.W; the `0x66` here comes from an
     * *embedded* prefix byte in the row's own op array, the same
     * mechanism `CvtIntToF`'s `Pf2`/`Pf3` embedding already uses, not
     * from `prefix66`/`width`). *)
    | PsllQXmm (r, imm) ->
        let rm = RReg (A.R (xreg_num r)) in
        let bytes = [0x66] @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:6 ~rm
                    @ [0x0f; 0x73] @ encode_rm 6 rm @ [imm land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zo_m (shift-by-1) -- goken's own `Yi1` class only
     * matches a *literal* immediate value of 1 (see `shift_by1_opcode`
     * comment) -- a genuinely different encoding (no immediate byte)
     * from any other constant, not just "1" happening to fit some
     * narrower range the way Arith's own imm8-vs-imm32 split works. *)
    | Shift (width, op, ShiftImm 1, dest) ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(shift_ext op) ~rm
                    @ [shift_by1_opcode width] @ encode_rm (shift_ext op) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zibo_m (shift-by-immediate-N). *)
    | Shift (width, op, ShiftImm v, dest) ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(shift_ext op) ~rm
                    @ [shift_byimm_opcode width] @ encode_rm (shift_ext op) rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zo_m (shift-by-CL/CX) -- goken's own y-table only
     * has a `Ycl`/`Ycx` row, no general `Yrl` one (real amd64 can only
     * ever shift by CL), confirmed: any other register fails at `6l`
     * with "notfound" -- guarded here the same way. *)
    | Shift (width, op, ShiftReg r, dest) when reg_num r = 1 (* CX *) ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(shift_ext op) ~rm
                    @ [shift_bycl_opcode width] @ encode_rm (shift_ext op) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Shift (_, _, ShiftReg _, _) ->
        raise Todo (* only CX is a valid shift-amount register in real amd64, see prelude *)

    (* claude: case Zmb_r / Zm_r -- sign/zero-extending "widening move"
     * -- see Ast_asm6.ml's `Extend` comment and `extend_shape`'s own
     * comment for the per-mnemonic opcode/REX story. *)
    | Extend (op, src, dst) ->
        let (need_w, byte_source, opcode_bytes) = extend_shape op in
        (* claude: resolve_gen_full, not resolve_gen -- real port_ctype.c's
         * own "_ctype" is a DATA-segment (SData2) global, not a TEXT
         * one, reachable here via a scaled-index source
         * ("MOVBLZX _ctype+0(SB)(CX*1),AX", fmt/strtod.c) -- see
         * `resolve_gen`'s own SData2 comment for why the plain version
         * can't handle it. *)
        let rm = resolve_gen_full env init_data node src in
        let bytes = extend_rex ~need_w ~byte_source ~reg_field:(reg_num dst) ~rm
                    @ opcode_bytes @ encode_rm (reg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zo_m -- NEG/NOT/INC/DEC, single ModRM-extension-
     * group operand, no immediate at all (the same Z-code `Shift`'s
     * own shift-by-1/shift-by-CL cases already use). *)
    | Unary (width, op, dest) ->
        (* claude: resolve_gen_full, not resolve_gen -- real fmt/fmt.c's
         * own "INCL fmtalloc<>+0(SB)" (a DATA-segment/SData2 allocation-
         * pool cursor, incremented in place) needs it, see `Extend`'s
         * own analogous comment above. *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(unary_ext op) ~rm
                    @ [unary_opcode width op] @ encode_rm (unary_ext op) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_o -- single-operand MUL/IMUL/DIV/IDIV, implicit
     * AX(:DX) -- structurally the mirror image of Zo_m above (the
     * operand plays goken's own `from` role, but `encode_rm`/`gen`
     * resolution doesn't care which AST field it came from). *)
    | MulDiv (width, op, src) ->
        let rm = resolve_gen_full env init_data node src in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:false ~width ~reg_field:(muldiv_ext op) ~rm
                    @ [muldiv_opcode width] @ encode_rm (muldiv_ext op) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r -- IMUL's own 2-operand form, opcode `0x0f
     * 0xaf` (ModRM.reg=dst, ModRM.rm=src -- the same load-shaped role
     * assignment `Move`'s own load clause uses). *)
    | Imul2 (width, src, dst) ->
        let rm = resolve_gen_full env init_data node src in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num dst) ~rm
                    @ [0x0f; 0xaf] @ encode_rm (reg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zlit -- CWD/CDQ/CQO, a single fixed opcode byte
     * (0x99) with a width-selecting prefix (Pe/none/Pw). *)
    | Cwd -> { size = 2; binary = (fun () -> [0x66; 0x99]) }
    | Cdq -> { size = 1; binary = (fun () -> [0x99]) }
    | Cqo -> { size = 2; binary = (fun () -> [0x48; 0x99]) }
  | _ -> raise (Impossible "arith_rules: not an arithmetic instruction")

let move_rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node)
    (instr : instr) : action =
  match instr with
    | Move (width, Either.Left (GReg r), dest) ->
        (* claude: case Zr_m -- store: reg -> mem/reg, goken's Zr_m
         * (0x89, or 0x88 for B_ -- see `mov_store_opcode`) -- same
         * shape for MOVQ/MOVL/MOVW/MOVB alike. *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [mov_store_opcode width] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (width, Either.Left src, GReg r) ->
        (* claude: case Zm_r -- load: mem/reg -> reg, goken's Zm_r
         * (0x8b, or 0x8a for B_ -- see `mov_load_opcode`) -- same for
         * all. *)
        let rm = resolve_gen_full env init_data node src in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [mov_load_opcode width] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (_, Either.Left (Indirect _ | Entity _ | IndirectScaled _ | EntityScaled _),
               (Indirect _ | Entity _ | IndirectScaled _ | EntityScaled _)) ->
        raise (Impossible "real amd64 MOV never has both operands in memory")
    (* claude: case Zclr -- "MOVQ/MOVL/MOVW $0,Rd" -- goken's Zclr
     * optimization (ymovq/ymovl/ymovw each have their own
     * "Yi0,Yrl,Zclr" row, ahead
     * of the general Ys32/Yi32 rows below -- a real bug in this port's
     * earlier assumption that only CMP's table lacked a Yi0 row and
     * MOVL's had one too like MOVQ's; found the hard way when a
     * fixture's own "MOVL $0,AX" didn't match goken's byte output).
     * Zclr reuses XOR's own reg-reg opcode (0x31) with the *same*
     * register in both the ModRM.reg and ModRM.rm fields ("XOR Rd,Rd"
     * -- self-XOR to zero), confirmed against real 6a for all three
     * widths. B_ is deliberately excluded -- ymovb's own table (see
     * Ast_asm6.ml's `width` comment) has *no* Yi0 row at all, so
     * "MOVB $0,AL" goes through the ordinary Zib_rp immediate path
     * below like any other byte immediate, confirmed against real 6a
     * ("MOVB $0,AL" -> "b0 00", not a self-XOR). *)
    | Move ((Q_ | L_ | W_) as width, Either.Right (A.Int 0), GReg r) ->
        let rm = RReg r in
        let bytes = prefix66 width @ rex_opt ~reg_is_register:true ~width ~reg_field:(reg_num r) ~rm
                    @ [0x31] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (Q_, Either.Right (A.Int v), dest) when v >= -0x8000_0000 && v <= 0x7fff_ffff ->
        (* claude: case Zilo_m -- immediate (sign-extends to 64-bit) ->
         * mem/reg, goken's Zilo_m (0xc7 /0) -- Ys32's own range only;
         * see the wider `Yi32` clause below for a value like
         * "$0xFFFFFFF6" that doesn't sign-extend correctly but still
         * fits 32 bits unsigned. *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = rex_opt ~reg_is_register:false ~width:Q_ ~reg_field:0 ~rm @ [0xc7] @ encode_rm 0 rm @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Ziq_rp (register, `l==0` downgrade) -- goken's own
     * Ziq_rp case body (span.c) checks `l = v>>32; if(l==0){ clear
     * REX.W; emit 0xb8+reg; put4(v); }` *before* falling to the
     * general 8-byte-immediate path -- a real gap in this port's own
     * earlier claim that Ziq_rp's internal special cases were
     * "provably unreachable for ymovq" (true only for the *other*
     * internal branch, `l==-1`; this one is genuinely reachable
     * whenever the immediate fits `Yi32` but not `Ys32` -- see
     * amd64_port.md's own "Real bugs/quirks"). Confirmed against
     * real 6a/6l: "MOVQ $0xFFFFFFF6,AX" -> `b8 f6 ff ff ff`, no REX at
     * all (REX.B still applies via `rex_opt`'s own `width:L_` for
     * R8-R15, confirmed: "MOVQ $0xFFFFFFF6,R9" -> `41 b9 f6 ff ff ff`). *)
    | Move (Q_, Either.Right (A.Int v), GReg r) when fits_yi32 v ->
        let bytes = rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm:(RReg r)
                    @ [0xb8 lor (reg_num r land 7)] @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zilo_m (memory, the wider `Yi32` row) -- only a
     * memory destination reaches here (a register one is already
     * claimed by `Ziq_rp`'s own downgrade above) -- opcode 0xc7,
     * REX.W still set (unlike the register case, there's no
     * downgrade for memory). Confirmed: "MOVQ $0xFFFFFFF6,-8(SP)" ->
     * `48 c7 44 24 f8 f6 ff ff ff`. *)
    | Move (Q_, Either.Right (A.Int v), dest) when fits_yi32 v ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = rex_opt ~reg_is_register:false ~width:Q_ ~reg_field:0 ~rm @ [0xc7] @ encode_rm 0 rm @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Ziq_rp (true 64-bit immediate) -- only reached now
     * when the immediate fits neither `Ys32` nor the wider `Yi32`
     * (i.e. genuinely needs more than 32 bits). Register destination
     * only (goken's own Yrl, not Yml -- no memory form of a genuine
     * 8-byte immediate move exists in real amd64 at all). Opcode is
     * 0xb8+reg (REX.B-extendable, same opcode-embedding family as
     * MOVL's own Zil_rp), *with* REX.W this time, followed by the
     * full 8-byte immediate. Confirmed against real 6a: "MOVQ
     * $0x123456789A,R9" -> "49 b9 9a 78 56 34 12 00 00 00". *)
    | Move (Q_, Either.Right (A.Int v), GReg r) ->
        let bytes = rex_opt ~reg_is_register:false ~width:Q_ ~reg_field:0 ~rm:(RReg r)
                    @ [0xb8 lor (reg_num r land 7)] @ le64 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zil_rp -- a lazily-resolved address-of-global value
     * (see Ast_asm6.ml's `imr`/Addr comment for the analogous CMP
     * case) as MOVQ's own immediate source -- real fmt/fmt.c's own
     * "MOVQ $fmtalloc<>+8(SB),DX". Confirmed against real 6a/6l: this
     * takes the *short* 32-bit form ("b8 <imm32>", no REX.W, the same
     * MOVL's own Zil_rp case below uses -- writing a 32-bit register
     * already zero-extends to 64 bits), not the full imm64 Ziq_rp form
     * `A.Int`'s own case above uses -- safe unconditionally (not just
     * "when it happens to fit," the way `A.Int`'s own value-dependent
     * range check works) because every address this port's whole non-
     * PIE addressing scheme ever produces already fits in 32 bits by
     * construction (see `encode_rm`'s own `RAbs` case: its disp32 is
     * `le32 addr`, unconditionally, no range check either -- the same
     * assumption, just applied here too). Eager size (5 bytes, always,
     * regardless of the resolved address's actual value) / lazy value
     * split, same as Lea's own Global case -- the size can't depend on
     * a value not known until `binary`'s own thunk runs. *)
    | Move (Q_, Either.Right (A.Address (A.Global (glob, off))), GReg r) ->
        let bytes_placeholder =
          rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm:(RReg r)
          @ [0xb8 lor (reg_num r land 7)] @ le32 0 in
        { size = List.length bytes_placeholder; binary = (fun () ->
            let addr = resolve_global_addr env init_data glob off in
            rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm:(RReg r)
            @ [0xb8 lor (reg_num r land 7)] @ le32 addr
          )
        }
    | Move (Q_, Either.Right _, _) ->
        raise Todo (* string/float src, an address to a *memory*
                     * destination, or an address to a Local/Param
                     * entity -- not wired, see prelude *)
    (* claude: case Zil_rp / Zilo_m -- MOVL's own immediate form is a
     * genuinely different shape from MOVQ's -- goken's ymovl table
     * puts "Yi32,Yrl,Zil_rp" (op+reg,
     * *no* ModRM, register embedded directly in the opcode byte, same
     * family as Ziq_rp) *before* "Yi32,Yml,Zilo_m" (0xc7 /0, *with*
     * ModRM), so a register destination takes the simpler Zil_rp path
     * and only a memory destination falls through to Zilo_m -- unlike
     * MOVQ, where both routes converge on Zilo_m regardless (see this
     * file's own prelude). Confirmed against real 6a: "MOVL $100,AX"
     * -> "b8 64 00 00 00" (5 bytes, no REX, no ModRM) and "MOVL $9,R9"
     * -> "41 b9 09 00 00 00" (REX.B extends the opcode's own embedded
     * register, exactly like ModRM.rm would). *)
    (* claude: `fits_yi32`, not just `Ys32`'s narrower range -- ymovl
     * has no Ys32/Yi32 split at all (a single `Yi32,Yrl,Zil_rp` row),
     * so a value like "$0xFFFFFFF6" is accepted the exact same way as
     * one in the sign-extendable range, same opcode either way.
     * Confirmed: "MOVL $0xFFFFFFF6,BX" -> `bb f6 ff ff ff`. *)
    | Move (L_, Either.Right (A.Int v), GReg r) when fits_yi32 v ->
        let bytes = rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm:(RReg r)
                    @ [0xb8 lor (reg_num r land 7)] @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (L_, Either.Right (A.Int v), dest) when fits_yi32 v ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm @ [0xc7] @ encode_rm 0 rm @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (L_, Either.Right _, _) ->
        raise Todo (* string/float src, or an immediate that doesn't
                     * fit 32 bits -- not wired, see prelude *)
    (* claude: case Zil_rp / Zilo_m (W_) -- MOVW mirrors MOVL's own
     * Zil_rp-for-register/Zilo_m-for-memory split exactly (goken's
     * ymovw table has the same row
     * shapes), just with a 2-byte immediate and the mandatory 0x66
     * prefix instead of REX.W. Confirmed against real 6a: "MOVW
     * $100,AX" -> "66 b8 64 00" (4 bytes: prefix+opcode+imm16, no
     * ModRM). `fits_yi32`, not just a 16-bit range -- goken's own
     * oclass() never looks at the destination instruction's width at
     * all, so even a value exceeding 32 bits' own relevant range is
     * accepted and simply truncated to the low 16 bits by `le16`'s own
     * masking. Confirmed: "MOVW $0x12345678,CX" -> `66 b9 78 56`
     * (imm16 = the low 16 bits, 0x5678). *)
    | Move (W_, Either.Right (A.Int v), GReg r) when fits_yi32 v ->
        let bytes = prefix66 W_ @ rex_opt ~reg_is_register:false ~width:W_ ~reg_field:0 ~rm:(RReg r)
                    @ [0xb8 lor (reg_num r land 7)] @ le16 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (W_, Either.Right (A.Int v), dest) when fits_yi32 v ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = prefix66 W_ @ rex_opt ~reg_is_register:false ~width:W_ ~reg_field:0 ~rm
                    @ [0xc7] @ encode_rm 0 rm @ le16 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (W_, Either.Right _, _) ->
        raise Todo (* string/float src, or an immediate that doesn't
                     * fit 16 bits -- not wired, see prelude *)
    (* claude: case Zib_rp / Zibo_m (B_) -- MOVB mirrors MOVL/MOVW's own
     * Zil_rp-for-register (here
     * goken's own Zib_rp -- same "op+reg, no ModRM" shape, just the
     * 0xb0+reg family instead of 0xb8+reg) / Zibo_m-for-memory (0xc6
     * /0, not 0xc7) split, with a single-byte immediate and no prefix
     * at all (see `prefix66`/`regrex_forces_rex` above -- REX is still
     * forced when the *register itself* is SP/BP/SI/DI, exactly as
     * for the ModRM-using cases). The full unsigned byte range is
     * accepted, same reasoning as Arith's own B_ clause: there's no
     * larger immediate form to fall back to for an 8-bit destination.
     * Confirmed against real 6a: "MOVB $200,SI" -> "40 b6 c8" (a bare,
     * otherwise-empty REX.40 forced to select SIL, not AH-family). *)
    | Move (B_, Either.Right (A.Int v), GReg r) when v >= -128 && v <= 255 ->
        let bytes = rex_opt ~reg_is_register:false ~width:B_ ~reg_field:0 ~rm:(RReg r)
                    @ [0xb0 lor (reg_num r land 7)] @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (B_, Either.Right (A.Int v), dest) when v >= -128 && v <= 255 ->
        let rm = resolve_gen_full env init_data node dest in
        let bytes = rex_opt ~reg_is_register:false ~width:B_ ~reg_field:0 ~rm
                    @ [0xc6] @ encode_rm 0 rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (B_, Either.Right _, _) ->
        raise Todo (* string/float src, or an immediate that doesn't
                     * fit 8 bits -- not wired, see prelude *)

    | Lea (Entity (A.Global (glob, off)), r) ->
        (* claude: case Zaut_r -- real address resolved lazily in `binary`'s own
         * thunk, not here -- a forward reference to a *TEXT* global
         * (e.g. "LEAQ later_proc(SB),R" naming a procedure declared
         * further down the same file) genuinely isn't in env.syms yet
         * during Layout6.ml's sizing pass, which populates each TEXT
         * symbol's own SText2 entry incrementally as it walks the
         * program -- confirmed the hard way (Not_found) with
         * indirect_call_jmp.s's own forward-referenced "exitnow". A
         * DATA global doesn't have this problem (Layout.layout_data
         * resolves the whole data segment upfront, before Layout6.ml
         * ever runs), but there's no way to tell which case a given
         * `glob` is without the same lookup that fails for TEXT --
         * so this is unconditionally deferred, same "eager size /
         * lazy value" split as Call/Jmp/Jcc's own real_pc-dependent
         * values. The size itself never depends on the resolved
         * address's actual numeric value (`encode_rm`'s RAbs case is
         * always the same 6-byte ModRM+SIB+disp32 shape), so an eager,
         * placeholder-free size computation is still safe. *)
        let rm_placeholder = RAbs 0 in
        let bytes_placeholder =
          rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(reg_num r) ~rm:rm_placeholder
          @ [0x8d] @ encode_rm (reg_num r) rm_placeholder in
        { size = List.length bytes_placeholder; binary = (fun () ->
            let addr = resolve_global_addr env init_data glob off in
            rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(reg_num r) ~rm:(RAbs addr)
            @ [0x8d] @ encode_rm (reg_num r) (RAbs addr)
          )
        }
    | Lea (EntityScaled (A.Global (glob, off), idx, scale), r) ->
        (* claude: same lazy-resolution case as the plain (no-index)
         * global LEA just above, plus a scaled index -- real
         * fmt/strtod.c's own "LEAQ tab1<>+0(SB)(CX*1),AX". *)
        let rm_placeholder = RAbsIndexed (0, idx, scale) in
        let bytes_placeholder =
          rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(reg_num r) ~rm:rm_placeholder
          @ [0x8d] @ encode_rm (reg_num r) rm_placeholder in
        { size = List.length bytes_placeholder; binary = (fun () ->
            let addr = resolve_global_addr env init_data glob off in
            let rm = RAbsIndexed (addr, idx, scale) in
            rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(reg_num r) ~rm
            @ [0x8d] @ encode_rm (reg_num r) rm
          )
        }
    | Lea ((GReg _ | Entity (A.Param _) | EntityScaled (A.Param _, _, _)), _) ->
        raise (Impossible "amd64 grammar never constructs LEA of a bare \
                            register or Entity(Param) -- see Ast_asm6.ml's Lea comment")
    | Lea (g, r) ->
        (* claude: case Zaut_r, every other `gen` shape -- register-
         * indirect, scaled-index, or FP-relative local. None of these
         * need the global case's own lazy resolution (the address is
         * already fully known: either a real register, or `env.autosize`
         * for a local, both available during Layout6.ml's sizing pass
         * too), so this is a plain eager encode, same shape as every
         * other `resolve_gen_full`-based instruction in this file. *)
        let rm = resolve_gen_full env init_data node g in
        let bytes = rex_opt ~reg_is_register:true ~width:Q_ ~reg_field:(reg_num r) ~rm
                    @ [0x8d] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
  | _ -> raise (Impossible "move_rules: not a move/lea instruction")

let control_flow_rules (node : 'a T.node) (instr : instr) : action =
  match instr with
    | Call { contents = A.IndirectJump r } ->
        (* claude: case Zo_m64 -- opcode 0xff /2, goken's ycall's
         * indirect form -- plain ModRM, no REX needed for a low register (confirmed
         * against real 6a: "CALL AX" -> "ff d0"). Reuses the shared
         * A.branch_operand's own IndirectJump constructor (already
         * produced by this arch's `branch: | ... | ireg { ref
         * (IndirectJump $1) }` rule, copied from ARM64's own grammar
         * template) rather than inventing a new AST case -- register-
         * indirect only, no memory-indirect form wired (goken's own
         * Yml class also accepts memory here, not implemented). *)
        (* claude: `width:L_` here only for its "REX.W=0" side effect
         * (see `rex_opt`) -- indirect CALL/JMP is *always* full
         * address-width in long mode regardless of any REX.W bit, it's
         * simply never needed (confirmed: no REX at all for a plain
         * low register). Not an actual 32-bit operation. *)
        let rm = RReg r in
        let bytes = rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm @ [0xff] @ encode_rm 2 rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Call _ ->
        (* claude: case Zcall -- opcode 0xe8 + rel32; goken's ycall's direct form. rel32 is
         * relative to the address right after this 5-byte
         * instruction -- real_pc isn't known yet during the sizing
         * pass (Layout6.ml calls this same `rules` before real_pc is
         * assigned to *later* nodes), but the SIZE (always exactly 5
         * bytes for a direct near call in 64-bit mode -- no shorter
         * encoding exists, unlike ARM's branch-range story) doesn't
         * depend on the actual displacement value, so this is safe to
         * compute eagerly; only `binary`'s own thunk, called after
         * layout has finished, actually reads node.branch/real_pc. *)
        { size = 5; binary = (fun () ->
            match node.T.branch with
            | None -> raise (Impossible "resolving should have set the branch field")
            | Some ndst ->
                let rel = ndst.T.real_pc - (node.T.real_pc + 5) in
                [0xe8] @ le32 rel
          )
        }
    (* claude: goken's yjmp/yjcond both real-relax between a 2-byte
     * short (rel8) and a 5/6-byte near (rel32) form based on the
     * actual distance -- see Ast_asm6.ml's own Jmp/Jcc comment and
     * Layout6.ml's own `relax` comment for the fixed-point that
     * decides this ahead of time. Dispatches on the extracted `bool`
     * (`is_long`), not a nested `{contents = ...}` pattern directly on
     * the `bool ref` -- pattern-matching that ref's shape inline
     * inside this same giant `instr` match reproducibly SIGSEGV'd on
     * this aarch64 host stress-testing a real 35-file closure (never
     * root-caused beyond that; reading the ref's value through an
     * ordinary `!` dereference first, before branching on the plain
     * `bool`, made the crash disappear). *)
    | Jmp (opd, is_long_ref) ->
        let is_long = !is_long_ref in
        (match !opd with
        | A.IndirectJump r ->
            let rm = RReg r in
            let bytes = rex_opt ~reg_is_register:false ~width:L_ ~reg_field:0 ~rm @ [0xff] @ encode_rm 4 rm in
            { size = List.length bytes; binary = (fun () -> bytes) }
        | _ when not is_long ->
            (* claude: case Zjmp -- direct JMP, short (rel8) form. *)
            { size = 2; binary = (fun () ->
                match node.T.branch with
                | None -> raise (Impossible "resolving should have set the branch field")
                | Some ndst ->
                    let rel = ndst.T.real_pc - (node.T.real_pc + 2) in
                    if rel < -128 || rel > 127
                    then raise (Impossible "Layout6.ml's relaxation should have forced the near form");
                    [0xeb; rel land 0xff]
              )
            }
        | _ ->
            (* claude: case Zjmpi -- direct JMP, near (rel32) form --
             * confirmed against real 6a/6l: "JMP <far>" -> "e9
             * <rel32>". *)
            { size = 5; binary = (fun () ->
                match node.T.branch with
                | None -> raise (Impossible "resolving should have set the branch field")
                | Some ndst ->
                    let rel = ndst.T.real_pc - (node.T.real_pc + 5) in
                    [0xe9] @ le32 rel
              )
            }
        )
    | Jcc (cond, _, is_long_ref) ->
        let is_long = !is_long_ref in
        if not is_long
        then
          (* claude: case Zbr -- Jcc, short (rel8) form. *)
          { size = 2; binary = (fun () ->
              match node.T.branch with
              | None -> raise (Impossible "resolving should have set the branch field")
              | Some ndst ->
                  let rel = ndst.T.real_pc - (node.T.real_pc + 2) in
                  if rel < -128 || rel > 127
                  then raise (Impossible "Layout6.ml's relaxation should have forced the near form");
                  [jcc_short_opcode cond; rel land 0xff]
            )
          }
        else
          (* claude: case Zbri -- Jcc, near (rel32) form -- 0x0f-
           * escaped, confirmed against real 6a/6l: "JEQ <far>" -> "0f
           * 84 <rel32>" (`jcc_short_opcode`'s own 0x74 plus 0x10,
           * matching goken's own optab.c AJEQ row "0x74,0x84" -- every
           * condition's near opcode is its own short opcode plus
           * 0x10, confirmed for the full AJxx family). *)
          { size = 6; binary = (fun () ->
              match node.T.branch with
              | None -> raise (Impossible "resolving should have set the branch field")
              | Some ndst ->
                  let rel = ndst.T.real_pc - (node.T.real_pc + 6) in
                  [0x0f; jcc_short_opcode cond + 0x10] @ le32 rel
            )
          }
    (* claude: case Zlit (ynone's own row -- RET is a single fixed
     * opcode byte, no operand) -- RET is control flow too (goken's own
     * case shape puts it right after CALL) -- kept in this section
     * rather than its own, matching Codegen7.ml's own RET placement
     * right after B/BL/Bxx. *)
    | Ret -> { size = 1; binary = (fun () -> [0xc3]) }
  | _ -> raise (Impossible "control_flow_rules: not a control-flow instruction")

let float_rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node)
    (instr : instr) : action =
  match instr with
    (* claude: case Zm_r_xm (load direction) -- goken's yxmov-shaped
     * MOVSD/MOVSS -- the *load* form (`Zm_r_xm`, opcode 0x10) is tried
     * first in goken's own table, so it wins even for a plain
     * register-to-register move (confirmed against real 6a/6l: "MOVSD
     * X0,X1" -> `f2 0f 10 c8`, not the store opcode) -- the *opposite*
     * clause order from `Move`'s own store-first split above, see
     * Ast_asm6.ml's `MovF` comment. No REX.W either precision. *)
    | MovF (prec, src, XReg r) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen src) in
        let bytes = [sse_prefix prec] @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num r) ~rm
                    @ [0x0f; 0x10] @ encode_rm (xreg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zr_m_xm (store direction) -- only reached when the
     * destination isn't a register (the load clause above already
     * claims every `XReg` destination, reg-reg included). *)
    | MovF (prec, XReg r, dest) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen dest) in
        let bytes = [sse_prefix prec] @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num r) ~rm
                    @ [0x0f; 0x11] @ encode_rm (xreg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | MovF (_, (XIndirect _ | XEntity _ | XIndirectScaled _ | XEntityScaled _),
               (XIndirect _ | XEntity _ | XIndirectScaled _ | XEntityScaled _)) ->
        raise (Impossible "real amd64 MOVSD/MOVSS never has both operands in memory")
    | MovF (_, XFloatImm _, _) | MovF (_, _, XFloatImm _) ->
        raise (Impossible "XFloatImm must be eliminated by Rewrite6.rewrite \
                            before Codegen6.gen ever runs -- see Ast_asm6.ml's own comment")

    (* claude: case Zm_r_xm -- goken's yxm-shaped dyadic SSE arithmetic
     * -- real x86's own in-place 2-operand shape ("dst := dst op
     * src"), ModRM.reg is always the destination, ModRM.rm the source
     * (confirmed: "ADDSD X1,X0" -> `f2 0f 58 c1`, reg=X0, rm=X1). No
     * REX.W either precision. *)
    | ArithF (op, prec, src, dst) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen src) in
        let bytes = [sse_prefix prec] @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num dst) ~rm
                    @ [0x0f; arithf_opcode_byte op] @ encode_rm (xreg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r_xm -- goken's yxcmp-shaped UCOMISD/UCOMISS --
     * see `ucomis_prefix`'s own comment for why this pair's prefix
     * story is a genuine third case, different from every other SSE
     * instruction here. Sets integer EFLAGS the same way an unsigned
     * CMP does -- see Ast_asm6.ml's `CmpF` comment for why this port
     * reuses the existing unsigned `Jcc` conditions as-is afterward. *)
    | CmpF (prec, src, dst) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen src) in
        let bytes = ucomis_prefix prec @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num dst) ~rm
                    @ [0x0f; 0x2e] @ encode_rm (xreg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r_xm -- goken's yxcvlf/yxcvqf-shaped
     * CVTS{L,Q}2S{D,S} (32-/64-bit int -> float) -- REX.W tracks the
     * *int* width, not the float precision (confirmed against real
     * 6a/6l: "CVTSQ2SD AX,X3" -> `f2 48 0f 2a d8` vs "CVTSL2SD AX,X0"
     * -> `f2 0f 2a c0`, no REX at all -- REX.B for R8-R15 still
     * applies to either, via `rex_opt`'s own `rm`-side check
     * regardless of `width`). `int_width` is always `Q_` or `L_`
     * here, never `W_`/`B_` -- grammar/parser never construct those. *)
    | CvtIntToF (int_width, prec, src, dst) ->
        let rm = resolve_gen_full env init_data node src in
        let bytes = [sse_prefix prec] @ rex_opt ~reg_is_register:true ~width:int_width ~reg_field:(xreg_num dst) ~rm
                    @ [0x0f; 0x2a] @ encode_rm (xreg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    (* claude: case Zm_r_xm -- goken's yxcvfq/yxcvfl-shaped
     * CVTTS{D,S}2S{Q,L} (float -> 32-/64-bit int, truncating) -- same
     * int-width-tracks-REX.W story. Confirmed: "CVTTSD2SQ X3,BX" ->
     * `f2 48 0f 2c db` vs "CVTTSD2SL X0,BX" -> `f2 0f 2c d8`. *)
    | CvtFToInt (int_width, prec, src, dst) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen src) in
        let bytes = [sse_prefix prec] @ rex_opt ~reg_is_register:true ~width:int_width ~reg_field:(reg_num dst) ~rm
                    @ [0x0f; 0x2c] @ encode_rm (reg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r_xm -- goken's yxm-shaped CVTSD2SS/CVTSS2SD --
     * one real opcode (0x5a) both directions, `sse_prefix`'s own
     * argument here is the *source* precision (see Ast_asm6.ml's
     * CvtFPrec comment), no REX.W either direction. *)
    | CvtFPrec (src_prec, src, dst) ->
        let rm = resolve_gen_full env init_data node (gen_of_xgen src) in
        let bytes = [sse_prefix src_prec] @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num dst) ~rm
                    @ [0x0f; 0x5a] @ encode_rm (xreg_num dst) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: case Zm_r_xm -- goken's yxm-shaped XORPD/XORPS, only the
     * self-XOR-to-zero idiom (see Ast_asm6.ml's XorClearF comment) --
     * XORPD's own prefix is `Pe` (0x66), XORPS's is `Pm` (no real
     * prefix at all). *)
    | XorClearF (prec, r) ->
        let rm = RReg (A.R (xreg_num r)) in
        let opt_prefix = match prec with A.D -> [0x66] | A.F -> [] in
        let bytes = opt_prefix @ rex_opt ~reg_is_register:true ~width:L_ ~reg_field:(xreg_num r) ~rm
                    @ [0x0f; 0x57] @ encode_rm (xreg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* claude: case Zlit (ynone's own row, same as RET above). *)
    | Syscall -> { size = 2; binary = (fun () -> [0x0f; 0x05]) }
  | _ -> raise (Impossible "float_rules: not a floating point/syscall instruction")

let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node)
    : action =
  (* claude: split into per-section helper functions (arith_rules/
   * move_rules/control_flow_rules/float_rules) rather than one giant
   * match here -- found the hard way stress-testing real lib_core/libc:
   * a single ~850-line `rules` match (this port's own real optab.c-
   * sized dispatch, after every arch-specific addressing mode this
   * session's hello_libc closure needed) reproducibly SIGSEGV'd on
   * this aarch64 host partway through Layout6.ml's very first sizing
   * pass over a real 35-file closure -- never inside any specific
   * case's own logic (confirmed: adding a print statement inside the
   * suspect Jmp arms never even printed before the crash), consistent
   * with the *dispatch* itself, not any instruction's encoding, being
   * the actual problem. Splitting the giant match into four smaller
   * ones (this dispatch, then each section's own self-contained match)
   * made the crash disappear -- never fully root-caused beyond that
   * (a plausible culprit: OCaml 4.14's non-flambda aarch64 backend
   * apparently doesn't always account for its own decision-tree's
   * total reach when choosing between a short and long conditional
   * branch encoding for a match this large, silently emitting an
   * out-of-range branch -- but this is inference from the empirical
   * fix, not a confirmed compiler bug reference). *)
  match node.T.instr with
  | T.Virt _ ->
      raise (Impossible "rewrite should have transformed virtual instrs")
  | T.TEXT (_, _, _) ->
      { size = 0; binary = (fun () -> []) }
  | T.WORD _ ->
      (* claude: no literal pool on this arch (see Ast_asm6.ml's
       * prelude) -- WORD is never emitted by Rewrite6.ml/Layout6.ml,
       * so this is unreachable for this checkpoint. *)
      raise Todo
  | T.I instr ->
    (match instr with
     | Arith _ | Cmp _ | Test _ | CmpXchg _ | Lock
     | MovQToXmm _ | MovQFromXmm _ | PsllQXmm _
     | Shift _ | Extend _ | Unary _ | MulDiv _ | Imul2 _ | Cwd | Cdq | Cqo ->
         arith_rules env init_data node instr
     | Move _ | Lea _ ->
         move_rules env init_data node instr
     | Call _ | Jmp _ | Jcc _ | Ret ->
         control_flow_rules node instr
     | MovF _ | ArithF _ | CmpF _ | CvtIntToF _ | CvtFToInt _ | CvtFPrec _
     | XorClearF _ | Syscall ->
         float_rules env init_data node instr
    )

let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int =
  (rules env None node).size

let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
    (cg : 'a T.code_graph) : T.byte array =
  let res = ref [] in
  let autosize = ref 0 in
  let pc = ref config.init_text in

  cg |> T.iter (fun n ->
    let { size; binary } =
        rules Codegen.{ syms = symbols2; autosize = !autosize }
        config.init_data n
    in
    let bytes = binary () in

    if n.T.real_pc <> !pc
    then raise (Impossible "Phase error, layout inconsistent with codegen");
    if List.length bytes <> size
    then raise (Impossible (spf "size of rule does not match #bytes at %s"
                              (T.s_of_loc n.T.n_loc)));

    (match n.T.instr with
     | T.TEXT (_, _, size) -> autosize := size
     | _ -> ());

    res := List.rev_append bytes !res;
    pc := !pc + size;
  );
  !res |> List.rev_map Char.chr |> Array.of_list
