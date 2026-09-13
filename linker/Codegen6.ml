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
 * docs/claude_notes/notes_amd64_port_plan.txt) rather than derived from
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
 * Scope for this first checkpoint (hello_linux_amd64.s only -- see
 * Ast_asm6.ml's own prelude): only the specific operand-class
 * combinations that fixture actually uses are implemented; every
 * unhandled shape raises Todo rather than silently emitting wrong
 * bytes (same convention as every other arch's own "not wired yet"
 * gaps). In particular:
 *  - Arith's immediate form only handles an immediate that fits a
 *    signed 8 bits (goken's Yi8 class, opcode 0x83) -- the imm32 form
 *    (opcode 0x81) isn't wired.
 *  - Move's immediate form only handles an immediate that fits signed
 *    32 bits sign-extended (goken's Ys32/Yi32 classes, opcode 0xc7) --
 *    the true-64-bit-immediate form (opcode 0xb8, goken's Ziq_rp) and
 *    the immediate-zero optimization (opcode 0x31, goken's Zclr)
 *    aren't wired.
 *  - Memory operands only support SP as the base register (`Indirect`)
 *    -- goken's own asmandsz() has real special cases for BP/R13 as a
 *    base (mod=00/rm=101 means RIP-relative/absolute instead of
 *    "[BP+0]" in 64-bit mode) that aren't replicated.
 *  - No SIB-index (indexed addressing, e.g. "(R1)(R2*4)") -- REX.X is
 *    always 0 here. R8-R15 *are* wired (REX.R/.B, see the `rex`/
 *    `rex_b_of_resolved_gen` helpers below) for every ModRM.reg/rm
 *    role every encoder already uses; only the SIB.index role (not
 *    used by anything wired so far -- see prelude) is unhandled.
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

let modrm ~md ~reg ~rm = ((md land 3) lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7)
let sib ~scale ~index ~base = ((scale land 3) lsl 6) lor ((index land 7) lsl 3) lor (base land 7)

let le32 (v : int) : int list =
  [ v land 0xff; (v asr 8) land 0xff; (v asr 16) land 0xff; (v asr 24) land 0xff ]

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

(* claude: REX prefix -- 0x40 | W<<3 | R<<2 | X<<1 | B. Ported from
 * goken's own obj.c reg[]/regrex[] table init (span.c/obj.c: register
 * *encoding* is `(i - D_AX) & 7` uniformly for AX..DI *and* R8..R15 --
 * already handled by `reg_num` returning the raw 0-15 value and
 * `modrm`/`sib` masking with `land 7` -- but R8-R15 additionally set
 * Rxr|Rxx|Rxb in regrex[], meaning "this register may need any of
 * REX.R/.X/.B depending on which ModRM/SIB field it ends up in").
 * W is always 1 here (every instruction wired so far is 64-bit/Q); X
 * (SIB index) is always 0 (no indexed addressing implemented, see
 * prelude); R comes from whichever register sits in ModRM.reg
 * (`reg_field` below -- for the immediate-group opcodes this is a
 * fixed 0-7 opcode-extension digit, never actually a register, so it
 * never contributes); B comes from whichever register sits in
 * ModRM.rm or SIB.base (`rex_b_of_resolved_gen` below -- RAbs's fixed
 * no-base SIB pattern never contributes either). *)
let rex_b_of_resolved_gen = function
  | RReg (A.R n) | RMem (A.R n, _) -> if n >= 8 then 1 else 0
  | RAbs _ -> 0

let rex ~(reg_field : int) ~(rm : resolved_gen) : int =
  let r = if reg_field >= 8 then 4 (* Rxr *) else 0 in
  let b = rex_b_of_resolved_gen rm in
  0x40 lor 8 (* W *) lor r lor b

(* claude: `init_data` is `None` here (as opposed to `resolve_gen_full`
 * below) -- callers that only ever pass a `gen` built from this arch's
 * own grammar without a `Global` case (currently just `Arith`'s dest,
 * see Ast_asm6.ml: hello_linux_amd64.s never targets a global directly
 * with ADD/SUB/XOR) can use this simpler version; anything that might
 * see `Entity (A.Global ...)` (Move's src/dst) must use
 * `resolve_gen_full` instead, which threads init_data through so a
 * SData2 (data-segment) global resolves correctly. *)
let resolve_gen (env : Codegen.env) (node : 'a T.node) (g : gen) : resolved_gen =
  match g with
  | GReg r -> RReg r
  | Indirect (r, off) -> RMem (r, off)
  | Entity (A.Global (glob, off)) ->
      (match Hashtbl.find env.syms (T.symbol_of_global glob) with
      | T.SText2 real_pc -> RAbs (real_pc + off)
      | T.SData2 _ ->
          raise (Impossible
            (spf "amd64: a SData global needs init_data -- use \
                  resolve_gen_full, not resolve_gen, at %s"
                  (T.s_of_loc node.T.n_loc)))
      )
  | Entity (A.Local (_, off)) ->
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
      RMem (rSP, env.autosize + 8 + off)
  | Entity (A.Param _) ->
      raise (Impossible
        (spf "amd64 grammar never constructs Entity(Param) -- see \
              Ast_asm6.ml's prelude (bare SP is a real register here, \
              not a virtual pseudo-register) at %s" (T.s_of_loc node.T.n_loc)))

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
  | RMem (_, _) ->
      raise Todo (* only SP as a memory base is wired, see prelude *)
  | RAbs addr ->
      (* claude: mod=00/rm=100(SIB)/SIB=no-index,no-base(base=101) ->
       * absolute disp32 -- goken's own non-PIE amd64 addressing
       * convention for D_EXTERN/D_STATIC symbols (span.c's asmandsz()
       * "temporary" comment: real bytes confirmed for "LEAQ msg(SB),AX"
       * -> "48 8d 04 25 <disp32>"). *)
      [ modrm ~md:0 ~reg:reg_field ~rm:4; sib ~scale:0 ~index:4 ~base:5 ] @ le32 addr

let arith_ext = function ADD -> 0 | SUB -> 5 | XOR -> 6
let arith_rr_opcode = function ADD -> 0x01 | SUB -> 0x29 | XOR -> 0x31

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
  | _ -> resolve_gen env node g

let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node)
    : action =
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

    (* --------------------------------------------------------------------- *)
    (* Arithmetic *)
    (* --------------------------------------------------------------------- *)

    | Arith (op, Imm v, dest) when v >= -128 && v < 128 ->
        let rm = resolve_gen env node dest in
        let bytes = [rex ~reg_field:(arith_ext op) ~rm; 0x83] @ encode_rm (arith_ext op) rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Arith (_op, Imm _, _dest) ->
        raise Todo (* imm32 form (opcode 0x81) not wired, see prelude *)
    | Arith (op, Reg r, dest) ->
        let rm = resolve_gen env node dest in
        let bytes = [rex ~reg_field:(reg_num r) ~rm; arith_rr_opcode op] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* claude: "CMPQ gen,$imm" -- goken's Zm_ibo case, opcode 0x83 /7,
     * `gen` in ModRM r/m (see Ast_asm6.ml's Cmp comment: the ModRM
     * operand is `gen` here even though it's the *first* operand,
     * unlike Arith's own Zibo_m where `gen` -- the *second* operand --
     * plays that role). Only the imm8 form is wired, same scope
     * restriction as Arith's own immediate case. *)
    | Cmp (g, Imm v) when v >= -128 && v < 128 ->
        let rm = resolve_gen env node g in
        let bytes = [rex ~reg_field:7 ~rm; 0x83] @ encode_rm 7 rm @ [v land 0xff] in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Cmp (_g, Imm _) ->
        raise Todo (* imm32 form (opcode 0x81) not wired, see prelude *)
    (* claude: "CMPQ gen,Rs" -- goken's Zm_r case, opcode 0x39, `gen`
     * in ModRM r/m, Rs in ModRM reg (asmand(from=gen,to=Rs), matching
     * Ast_asm6.ml's Cmp comment). The reverse-direction row (Zr_m,
     * opcode 0x3b, for when `gen` should land in ModRM reg instead) is
     * a separate goken y-table row this port doesn't need yet -- not
     * wired. *)
    | Cmp (g, Reg r) ->
        let rm = resolve_gen env node g in
        let bytes = [rex ~reg_field:(reg_num r) ~rm; 0x39] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* --------------------------------------------------------------------- *)
    (* Memory / Move *)
    (* --------------------------------------------------------------------- *)

    | Move (Q_, Either.Left (GReg r), dest) ->
        (* store: reg -> mem/reg, goken's Zr_m (0x89) *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = [rex ~reg_field:(reg_num r) ~rm; 0x89] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (Q_, Either.Left src, GReg r) ->
        (* load: mem/reg -> reg, goken's Zm_r (0x8b) *)
        let rm = resolve_gen_full env init_data node src in
        let bytes = [rex ~reg_field:(reg_num r) ~rm; 0x8b] @ encode_rm (reg_num r) rm in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (Q_, Either.Left (Indirect _ | Entity _), (Indirect _ | Entity _)) ->
        raise (Impossible "real amd64 MOV never has both operands in memory")
    | Move (Q_, Either.Right (A.Int v), dest) when v >= -0x8000_0000 && v <= 0x7fff_ffff ->
        (* immediate (sign-extends to 64-bit) -> mem/reg, goken's
         * Zilo_m (0xc7 /0) -- see prelude for the imm=0/true-imm64
         * cases not wired. *)
        let rm = resolve_gen_full env init_data node dest in
        let bytes = [rex ~reg_field:0 ~rm; 0xc7] @ encode_rm 0 rm @ le32 v in
        { size = List.length bytes; binary = (fun () -> bytes) }
    | Move (Q_, Either.Right _, _) ->
        raise Todo (* true-64-bit-immediate / string / float src, or an
                     * immediate too big for sign-extended-32-bit --
                     * not wired, see prelude *)

    | Lea (glob, off, r) ->
        let addr = resolve_global_addr env init_data glob off in
        let bytes = [rex ~reg_field:(reg_num r) ~rm:(RAbs addr); 0x8d] @ encode_rm (reg_num r) (RAbs addr) in
        { size = List.length bytes; binary = (fun () -> bytes) }

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    | Call _ ->
        (* opcode 0xe8 + rel32; goken's ycall's direct form. rel32 is
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
     * actual distance (span.c's Zjmp/Zbr cases) -- genuinely a
     * multi-pass sizing problem (this instruction's own SIZE depends
     * on a distance only known once every node's real_pc is assigned,
     * which itself depends on every instruction's size -- the same
     * kind of chicken-and-egg problem ARM32/MIPS/RISC-V's own branch-
     * range stories needed real relaxation passes for). Only the short
     * form is wired here: a real, if surprising, *second* obstacle
     * to a naive "always emit the near form and let goken match it"
     * plan (which would have sufficed for sizing alone) is that
     * goken's own linker deletes any code that's unreachable except by
     * falling through an unconditional jump that skips it entirely,
     * *and* deletes the now-redundant jump itself once its target
     * becomes the next real instruction (confirmed empirically: a
     * "JMP L; <dead code>; L:" fixture assembles to nothing at all
     * for the JMP or the dead code) -- so artificially padding a
     * fixture's jump distances past 127 bytes to force goken's own
     * near form doesn't actually work either, unless the "dead" code
     * is made genuinely reachable some other way. Short form only is
     * therefore both simpler *and* the only form actually exercised by
     * realistic small fixtures; guarded (raise Todo) rather than
     * silently emitting a wrong rel8 if a future fixture's distance
     * doesn't fit -- checked lazily inside `binary`'s own thunk, since
     * real_pc isn't resolved yet during Layout6.ml's sizing pass (same
     * "eager size / lazy check" split RISC-V's own far-branch guard
     * uses). *)
    | Jmp _ ->
        { size = 2; binary = (fun () ->
            match node.T.branch with
            | None -> raise (Impossible "resolving should have set the branch field")
            | Some ndst ->
                let rel = ndst.T.real_pc - (node.T.real_pc + 2) in
                if rel < -128 || rel > 127
                then raise Todo (* target too far for the short form -- see prelude *);
                [0xeb; rel land 0xff]
          )
        }
    | Jcc (cond, _) ->
        { size = 2; binary = (fun () ->
            match node.T.branch with
            | None -> raise (Impossible "resolving should have set the branch field")
            | Some ndst ->
                let rel = ndst.T.real_pc - (node.T.real_pc + 2) in
                if rel < -128 || rel > 127
                then raise Todo (* target too far for the short form -- see prelude *);
                [jcc_short_opcode cond; rel land 0xff]
          )
        }
    (* claude: RET is control flow too (goken's own case shape puts it
     * right after CALL) -- kept in this section rather than its own,
     * matching Codegen7.ml's own RET placement right after B/BL/Bxx. *)
    | Ret -> { size = 1; binary = (fun () -> [0xc3]) }

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    | Syscall -> { size = 2; binary = (fun () -> [0x0f; 0x05]) }
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
