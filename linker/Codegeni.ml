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

module Str = Re_str
open Ast_asm
open Ast_asmi

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* RISC-V (RV32) code generation.
 *
 * The 'case <n>: ...' comments below refer to code in il/asm.c so one
 * can easily check the corresponding C code that was used as model
 * for the OCaml code (mirrors the convention in Codegen5.ml/
 * Codegenv.ml).
 *
 * Unlike ARM (needs a literal pool) or MIPS (needs "lu+or"), a large
 * RISC-V constant is always materialized inline at its use site via
 * LUI (+ADDI for the low 12 bits) -- see cases 8/9/20 below -- so
 * there is no pool/splicing mechanism here at all.
 *
 * goken's il also applies instruction *compression* (RVC, 16-bit
 * encodings for eligible instructions, see il/compress.c) unless
 * given `-c`; this port never emits compressed instructions, so the
 * differential harness always passes `-c` to goken's il to compare
 * apples to apples -- see scripts/diff-riscv.sh and
 * docs/claude_notes/notes_riscv_port_plan.txt.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error (node : 'a T.node) (s : string) =
  failwith
    (spf "%s at %s on %s" s (T.s_of_loc node.n_loc)
        (Typesi.show_instr node.instr))
let int_of_bits (n : 'a T.node) (x : Bits.int32) : int =
  try Bits.int_of_bits32 x with
  | Failure s -> error n s

(*****************************************************************************)
(* Constants and helpers *)
(*****************************************************************************)

(* claude: BIG, ported from goken's linkers/il/l.h. RSB (aka SB, aka
 * gp/x3 in the real ISA) is set up at program start to point BIG
 * bytes into the data segment (see "setSB" below), so a *later*
 * MOVW $sym(SB) can reach it with one `ADDI rd, RSB, offset-BIG`
 * instead of loading the full 32-bit absolute address via LUI+ADDI --
 * see cases 11 (fast path) and 20 (slow/absolute path) below.
 *
 * Unlike MIPS (where the analogous BIG is 0, permanently disabling
 * this fast path in goken itself -- see Codegenv.ml) RISC-V's BIG is
 * a real, live value: 2048, which is exactly the magnitude of a
 * 12-bit signed immediate's range. That's not a coincidence -- it's
 * what makes the fast-path encodability check below a plain range
 * check instead of ARM's bit-rotation search (immrot): any
 * offset-BIG that still fits a signed 12-bit immediate is reachable.
 *)
let big = 2048

let offset_to_SB x = x - big

(* claude: the exact condition il/span.c's aclass() uses (the D_ADDR/
 * SDATA case) is `instoffset >= -BIG && instoffset < BIG`, i.e. the
 * signed 12-bit immediate range -- equivalently, "fits in ADDI's
 * imm field". The separate `!= 0` exclusion (checked at the call
 * site below, not here) is what keeps `MOVW $setSB(SB), RSB` itself
 * from trying to use the fast path, which would be circular: RSB
 * isn't set up yet at that point.
 *)
let fits_addi_imm x = x >= -big && x < big

(*****************************************************************************)
(* Instruction encoding helpers *)
(*****************************************************************************)
(* Standard RV32I instruction formats (I/S/B/U/J-type); mirrors
 * goken's OP_* macros in il/asm.c, just spelled out as Bits.t
 * (value, bit-offset) lists like Codegen5.ml/Codegenv.ml do, instead
 * of C bit-shift expressions.
 *)

let op_opimm = 0x13 (* ADDI/SLTI/etc *)
let op_lui = 0x37
(* claude: AUIPC (PC-relative "add upper immediate"); riscv64/ojl uses
 * this instead of LUI for absolute-address computations -- see
 * gen_pcrelative and case 20 below. *)
let op_auipc = 0x17
let op_system = 0x73 (* ECALL/EBREAK *)

(* I-type: imm[31:20] rs1[19:15] funct3[14:12] rd[11:7] opcode[6:0] *)
let op_itype opcode funct3 (R rs1) (R rd) (imm : int) : Bits.t =
  [(opcode, 0); (rd, 7); (funct3, 12); (rs1, 15); (imm land 0xfff, 20)]

(* U-type: imm[31:12] rd[11:7] opcode[6:0]. `imm20` is the already
 * upper-20-bit-shifted value (as goken's `v&0xFFFFF000` is).
 *)
let op_utype opcode (R rd) (imm20 : int) : Bits.t =
  [(opcode, 0); (rd, 7); ((imm20 lsr 12) land 0xfffff, 12)]

(* claude: goken's case 9/20 pattern for materializing an absolute
 * 32-bit value v into rd: LUI the upper 20 bits, rounding up (adding
 * 0x1000) if bit 11 of v is set, since ADDI's 12-bit immediate is
 * *signed* -- ADDI rd,rd,(v&0xFFF as a signed 12-bit value) would
 * otherwise subtract instead of add when that low part's bit 11 is
 * set. Then ADDI the (now-consistent) low 12 bits into rd itself.
 *)
let gen_absolute_via (opcode : int) (rd : reg) (v : int) : Bits.t list =
  let v = if v land 0x800 <> 0 then v + 0x1000 else v in
  [ op_utype opcode rd v;
    op_itype op_opimm 0 rd rd (v land 0xfff);
  ]
let gen_absolute (rd : reg) (v : int) : Bits.t list = gen_absolute_via op_lui rd v

(* claude: same instruction pair as gen_absolute, but AUIPC instead of
 * LUI -- goken's riscv64 (thechar='j') uses this for case 12/13/18/20
 * (see il/asm.c's `thechar == 'j' ? OP_UP(...) : OP_U(...)`) so that
 * the materialized address stays correct as a *delta from this
 * instruction's own pc* rather than a 32-bit-truncated absolute
 * value. Caller is responsible for passing that delta as `v`. *)
let gen_pcrelative (rd : reg) (v : int) : Bits.t list = gen_absolute_via op_auipc rd v

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions:
 * - rf = register from (p->from.reg in il)
 * - rt = register to (p->to.reg in il)
 *)

(* claude: is_64 is only ever read inside a `binary` thunk (never
 * during the sizing pass -- see size_of_instruction below), mirroring
 * how init_data is threaded; it's riscv64/ojl's equivalent of goken's
 * global `thechar == 'j'` check. *)
let rules (is_64 : bool)
    (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  match node.instr with
  (* Reusable *)
  | T.Virt _ | T.TEXT _ | T.WORD _ ->
      Codegen.default_rules env init_data node

  | T.I instr ->
    (match instr with

    (* --------------------------------------------------------------------- *)
    (* Arithmetic *)
    (* --------------------------------------------------------------------- *)

    (* case 2:		/* addi $I,[R,]D */ *)
    | Arith (ADD None, Imm i, middle, rt) ->
        let (R r) = (match middle with Some x -> x | None -> rt) in
        if not (fits_addi_imm i)
        then error node "TODO: addi immediate out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm 0 (R r) rt i ]
          )}

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* case 24:		/* SYS *)
    | ECALL ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_system 0 rZERO rZERO 0 ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* claude: RISC-V has no branch-delay slot (unlike MIPS) -- see
     * docs/claude_notes/notes_riscv_port_plan.txt / notes_mips_port_plan.txt
     * for that story. RET (Rewritei.ml) expands to a plain
     * `JMP (RLINK)`, encoded as `JALR x0, 0(RLINK)` (rd=x0 means
     * "don't save a return address", i.e. an unconditional jump).
     *)
    | JMP { contents = (IndirectJump rt) } ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype 0x67 (* JALR opcode *) 0 rt rZERO 0 ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Memory / Address *)
    (* --------------------------------------------------------------------- *)

    (* case 2:		/* addi $I,[R,]D */ ("MOVW $imm,R"; the implicit
     * middle here is RZERO, not the destination, since this is the
     * MOV-immediate pseudo-op, not a general Arith -- same asymmetry
     * as ARM's Codegen5.ml (MOV/MVN default to r=0, other ops
     * default to r=destination); see also case 9 for imm too big to
     * fit in 12 bits.
     *)
    | Move2 (W__, Right (Int i), Gen (GReg rt)) ->
        if fits_addi_imm i
        then
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm 0 rZERO rt i ]
          )}
        else
          (* case 9:	/* lui I1,D; addi I0,D */ *)
          { size = 8; x = None; binary = (fun () -> gen_absolute rt i) }

    | Move2 (W__, Right (Float _), Gen (GReg _)) ->
        failwith "TODO: ?? because of refactor of imm_or_ximm"
    | Move2 (W__, Right (String _), Gen (GReg _)) ->
        (* stricter? what does il do with that? confusing I think *)
        error node "string not allowed in MOVW; use DATA"

    | Move2 (W__, Right (Address (Global (global, _offsetTODO))), Gen (GReg rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 real_pc ->
            (* address of a procedure: always the absolute-load path,
             * same as ARM/MIPS (a TEXT symbol isn't RSB-relative) *)
            { size = 8; x = None; binary = (fun () -> gen_absolute rt real_pc) }
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB offset in
            (* case 11:	/* addi $I,R,D */
             * super important condition! for bootstrapping setSB in
             * MOVW $setSB(SB), RSB and not transform it into
             * ADDI RSB, RSB, offset_to_SB (circular: RSB isn't set
             * up yet at that point). *)
            if final_offset <> 0 && fits_addi_imm final_offset
            then
              { size = 4; x = None; binary = (fun () ->
                [ op_itype op_opimm 0 rSB rt final_offset ]
              )}
            else
              (* case 20:	/* lui/auipc I1,D; addi I0; D */
               * absolute address = data-segment offset + INITDAT,
               * same formula ARM/MIPS's lcon fallback uses.
               * init_data isn't known yet during the sizing pass
               * (Layouti.layout_text calls size_of_instruction with
               * init_data=None -- only the *size* is needed then,
               * so the actual lookup must stay inside the binary
               * thunk, evaluated only during the later real gen
               * pass), same as Codegen.default_rules's WORD case.
               *)
              { size = 8; x = None; binary = (fun () ->
                match init_data with
                | None -> raise (Impossible "init_data should be set by now")
                | Some init_data ->
                    let target_abs = offset + init_data in
                    if is_64
                    then
                      (* claude: riscv64/ojl: `vv = regoff(&p->from) +
                       * instoffx - (pc + INITTEXT)` in il/asm.c's
                       * case 20 -- AUIPC encodes a delta from *this
                       * instruction's own* absolute pc, not the
                       * absolute address itself. Unlike goken's raw
                       * `pc` (text-relative, 0 at the first
                       * instruction, needing the explicit +INITTEXT),
                       * xix's node.real_pc is already absolute (see
                       * Layouti.layout_text: `pc := ref init_text`),
                       * so no extra +INITTEXT term is needed here. *)
                      let delta = target_abs - node.real_pc in
                      gen_pcrelative rt delta
                    else gen_absolute rt target_abs
              )}
        )

    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rbase, offset))) ->
        (* case 6:		/* sb R,I(S) */
         * store, needed by Rewritei.ml's link-register-save
         * prologue (`MOVW RLINK,0(SP)`). *)
        if not (fits_addi_imm offset)
        then error node "TODO: store offset out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            (* S-type: imm[31:25] rs2[24:20] rs1[19:15] funct3[14:12]
             * imm[11:7] opcode[6:0] *)
            let (R rs2) = rf and (R rs1) = rbase in
            [ [(0x23, 0); (offset land 0x1f, 7); (0, 12);
               (rs1, 15); (rs2, 20); ((offset lsr 5) land 0x7f, 25)] ]
          )}

    | Move2 (W__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        (* case 7:		/* lb I(S),D */
         * load, needed by Rewritei.ml's link-register-restore
         * epilogue (`MOVW 0(SP),RLINK`). *)
        if not (fits_addi_imm offset)
        then error node "TODO: load offset out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            [ op_itype 0x03 (* LOAD opcode *) 2 (* funct3=010=LW *)
                rbase rt offset ]
          )}

    (* --------------------------------------------------------------------- *)
    (* Other: not ported yet *)
    (* --------------------------------------------------------------------- *)
    | Arith _ | ArithMul _ | ArithF _ | LUI
    | Move1 _ | Move2 _
    | JMP _ | JAL _ | JALR _ | Bxx _
    | FENCE_I | BREAK | SYS
       ->
       failwith (spf "Codegeni: TODO: instr not handled: %s"
                (Typesi.show_instr node.instr))
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* must return a multiple of 4 *)
let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int =
  (* is_64 doesn't affect any instruction's *size* (AUIPC vs LUI is
   * still one 4-byte instruction either way), only the `binary`
   * thunk's contents -- never forced during sizing, so this dummy
   * value is never actually read. *)
  let action = rules false env None node in
  action.size

let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in

  let pc = ref config.init_text in

  cg |> T.iter (fun n ->

    let is_64 = (match config.arch with Arch.Riscv64 -> true | _ -> false) in
    let {size; binary; x = _} =
        rules is_64
        Codegen.{ syms = symbols2; autosize = !autosize }
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
                     (Typesi.show_instr n.instr) |> String_.show_max 40));
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.debug (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
    end;

    let xs = xs |> List.map (fun x -> int_of_bits n x) in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten
