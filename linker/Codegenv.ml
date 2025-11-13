(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

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

(* LATER? x - BIG optimisation *)
let offset_to_R30 x =
  x

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
        { size = 4; x = None; binary = (fun () -> 
           [ op_rrr op_jmp rZERO rt r ]
         ) }
    (* case 11:	/* jmp lbra */ *)
    | JAL { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ gbranch_static node true ]
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
            let from_part_when_small_offset_to_R30 =
              try 
                let v = Hashtbl.find env.syms (T.symbol_of_global global) in
                match v with
                | T.SData2 (offset, _kind) ->
                    let final_offset = offset_to_R30 offset in
                    (* super important condition! for bootstrapping
                     * setR30 in MOVW $setR30(SB), R30 and not
                     * transform it in ADD offset_set_R30, R30, R30.
                     *)
                    if final_offset = 0 
                    then None
                    else failwith "TODO: final_offset <> 0"
                | T.SText2 _real_pc -> None
              (* layout_text has not been fully done yet so we may have
               * the address of a procedure we don't know yet
               *)
              with Not_found -> None
            in
            (match from_part_when_small_offset_to_R30 with
            | Some _ ->
                 failwith "TODO: from_part_when_small_offset_to_R30 is a Some"
            | None ->

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
            )
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




