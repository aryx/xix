(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_vof.cmo  pr_o.cmo /tmp/xxx.ml  *)

open Ast_asm5

let vof_pos = Ocaml.vof_int
let vof_symbol = Ocaml.vof_string
let vof_label = Ocaml.vof_string
let vof_integer = Ocaml.vof_int
let vof_offset = Ocaml.vof_int
let vof_virt_pc = Ocaml.vof_int
let vof_filename = Ocaml.vof_string

let vof_register =
  function | R v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("R", [ v1 ]))


let vof_entity { name = v_name; priv = v_priv; signature = v_signature } =
  let bnds = [] in
  let arg = Ocaml.vof_option Ocaml.vof_int v_signature in
  let bnd = ("signature", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option Ocaml.vof_int v_priv in
  let bnd = ("priv", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_symbol v_name in
  let bnd = ("name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  
let rec vof_arith_operand =
  function
  | Imm v1 -> let v1 = vof_integer v1 in Ocaml.VSum (("Imm", [ v1 ]))
  | Reg v1 -> let v1 = vof_register v1 in Ocaml.VSum (("Reg", [ v1 ]))
  | Shift ((v1, v2, v3)) ->
      let v1 = vof_register v1
      and v2 = vof_shift_reg_op v2
      and v3 = Ocaml.vof_either vof_register Ocaml.vof_int v3
      in Ocaml.VSum (("Shift", [ v1; v2; v3 ]))
and vof_shift_reg_op =
  function
  | Sh_logic_left -> Ocaml.VSum (("Sh_logic_left", []))
  | Sh_logic_right -> Ocaml.VSum (("Sh_logic_right", []))
  | Sh_arith_right -> Ocaml.VSum (("Sh_arith_right", []))
  | Sh_rotate_right -> Ocaml.VSum (("Sh_rotate_right", []))
  
let rec vof_mov_operand =
  function
  | Imsr v1 -> let v1 = vof_arith_operand v1 in Ocaml.VSum (("Imsr", [ v1 ]))
  | Ximm v1 -> let v1 = vof_ximm v1 in Ocaml.VSum (("Ximm", [ v1 ]))
  | Indirect ((v1, v2)) ->
      let v1 = vof_register v1
      and v2 = vof_offset v2
      in Ocaml.VSum (("Indirect", [ v1; v2 ]))
  | Param ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_symbol v1
      and v2 = vof_offset v2
      in Ocaml.VSum (("Param", [ v1; v2 ]))
  | Local ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_symbol v1
      and v2 = vof_offset v2
      in Ocaml.VSum (("Local", [ v1; v2 ]))
  | Entity ((v1, v2)) ->
      let v1 = vof_entity v1
      and v2 = vof_offset v2
      in Ocaml.VSum (("Entity", [ v1; v2 ]))
and vof_ximm =
  function
  | String v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("String", [ v1 ]))
  | Address v1 -> let v1 = vof_entity v1 in Ocaml.VSum (("Address", [ v1 ]))
  
let rec vof_branch_operand v = Ocaml.vof_ref vof_branch_operand2 v
and vof_branch_operand2 =
  function
  | Relative v1 ->
      let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("Relative", [ v1 ]))
  | LabelUse ((v1, v2)) ->
      let v1 = vof_label v1
      and v2 = vof_offset v2
      in Ocaml.VSum (("LabelUse", [ v1; v2 ]))
  | SymbolJump v1 ->
      let v1 = vof_entity v1 in Ocaml.VSum (("SymbolJump", [ v1 ]))
  | Absolute v1 ->
      let v1 = vof_virt_pc v1 in Ocaml.VSum (("Absolute", [ v1 ]))
  | IndirectJump v1 ->
      let v1 = vof_register v1 in Ocaml.VSum (("IndirectJump", [ v1 ]))
  
let rec vof_instr =
  function
  | Arith ((v1, v2, v3, v4, v5)) ->
      let v1 = vof_arith_opcode v1
      and v2 = vof_arith_option v2
      and v3 = vof_arith_operand v3
      and v4 = Ocaml.vof_option vof_register v4
      and v5 = vof_register v5
      in Ocaml.VSum (("Arith", [ v1; v2; v3; v4; v5 ]))
  | MOVE ((v1, v2, v3, v4)) ->
      let v1 = vof_move_size v1
      and v2 = vof_move_option v2
      and v3 = vof_mov_operand v3
      and v4 = vof_mov_operand v4
      in Ocaml.VSum (("MOVE", [ v1; v2; v3; v4 ]))
  | SWAP ((v1, v2, v3, v4)) ->
      let v1 = vof_move_size v1
      and v2 = vof_register v2
      and v3 = vof_register v3
      and v4 = Ocaml.vof_option vof_register v4
      in Ocaml.VSum (("SWAP", [ v1; v2; v3; v4 ]))
  | B v1 -> let v1 = vof_branch_operand v1 in Ocaml.VSum (("B", [ v1 ]))
  | BL v1 -> let v1 = vof_branch_operand v1 in Ocaml.VSum (("BL", [ v1 ]))
  | RET -> Ocaml.VSum (("RET", []))
  | Cmp ((v1, v2, v3)) ->
      let v1 = vof_cmp_opcode v1
      and v2 = vof_arith_operand v2
      and v3 = vof_register v3
      in Ocaml.VSum (("Cmp", [ v1; v2; v3 ]))
  | Bxx ((v1, v2)) ->
      let v1 = vof_condition v1
      and v2 = vof_branch_operand v2
      in Ocaml.VSum (("Bxx", [ v1; v2 ]))
  | SWI v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("SWI", [ v1 ]))
  | RFE -> Ocaml.VSum (("RFE", []))
  | NOP -> Ocaml.VSum (("NOP", []))
and vof_arith_opcode =
  function
  | AND -> Ocaml.VSum (("AND", []))
  | ORR -> Ocaml.VSum (("ORR", []))
  | EOR -> Ocaml.VSum (("EOR", []))
  | ADD -> Ocaml.VSum (("ADD", []))
  | SUB -> Ocaml.VSum (("SUB", []))
  | MUL -> Ocaml.VSum (("MUL", []))
  | DIV -> Ocaml.VSum (("DIV", []))
  | MOD -> Ocaml.VSum (("MOD", []))
  | SLL -> Ocaml.VSum (("SLL", []))
  | SRL -> Ocaml.VSum (("SRL", []))
  | SRA -> Ocaml.VSum (("SRA", []))
  | BIC -> Ocaml.VSum (("BIC", []))
  | ADC -> Ocaml.VSum (("ADC", []))
  | SBC -> Ocaml.VSum (("SBC", []))
  | RSB -> Ocaml.VSum (("RSB", []))
  | RSC -> Ocaml.VSum (("RSC", []))
  | MOV -> Ocaml.VSum (("MOV", []))
  | MVN -> Ocaml.VSum (("MVN", []))
and vof_arith_option v = Ocaml.vof_option vof_arith_cond v
and vof_arith_cond =
  function | Set_condition -> Ocaml.VSum (("Set_condition", []))
and vof_cmp_opcode =
  function
  | CMP -> Ocaml.VSum (("CMP", []))
  | TST -> Ocaml.VSum (("TST", []))
  | TEQ -> Ocaml.VSum (("TEQ", []))
  | CMN -> Ocaml.VSum (("CMN", []))
and vof_condition =
  function
  | EQ -> Ocaml.VSum (("EQ", []))
  | NE -> Ocaml.VSum (("NE", []))
  | GT v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("GT", [ v1 ]))
  | LT v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("LT", [ v1 ]))
  | GE v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("GE", [ v1 ]))
  | LE v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("LE", [ v1 ]))
  | MI -> Ocaml.VSum (("MI", []))
  | PL -> Ocaml.VSum (("PL", []))
  | VS -> Ocaml.VSum (("VS", []))
  | VC -> Ocaml.VSum (("VC", []))
  | AL -> Ocaml.VSum (("AL", []))
  | NV -> Ocaml.VSum (("NV", []))
and vof_move_size =
  function
  | Word -> Ocaml.VSum (("Word", []))
  | HalfWord v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("HalfWord", [ v1 ]))
  | Byte v1 -> let v1 = vof_sign v1 in Ocaml.VSum (("Byte", [ v1 ]))
and vof_sign =
  function
  | Signed -> Ocaml.VSum (("Signed", []))
  | Unsigned -> Ocaml.VSum (("Unsigned", []))
and vof_move_option v = Ocaml.vof_option vof_move_cond v
and vof_move_cond =
  function
  | WriteAddressBase -> Ocaml.VSum (("WriteAddressBase", []))
  | PostOffsetWrite -> Ocaml.VSum (("PostOffsetWrite", []))
  
let rec vof_pseudo_instr =
  function
  | TEXT ((v1, v2, v3)) ->
      let v1 = vof_entity v1
      and v2 = vof_attributes v2
      and v3 = Ocaml.vof_int v3
      in Ocaml.VSum (("TEXT", [ v1; v2; v3 ]))
  | GLOBL ((v1, v2, v3)) ->
      let v1 = vof_entity v1
      and v2 = vof_attributes v2
      and v3 = Ocaml.vof_int v3
      in Ocaml.VSum (("GLOBL", [ v1; v2; v3 ]))
  | DATA ((v1, v2, v3, v4)) ->
      let v1 = vof_entity v1
      and v2 = vof_offset v2
      and v3 = Ocaml.vof_int v3
      and v4 = vof_imm_or_ximm v4
      in Ocaml.VSum (("DATA", [ v1; v2; v3; v4 ]))
  | WORD v1 -> let v1 = vof_imm_or_ximm v1 in Ocaml.VSum (("WORD", [ v1 ]))
and vof_attributes v = Ocaml.vof_list vof_attribute v
and vof_attribute =
  function
  | DUPOK -> Ocaml.VSum (("DUPOK", []))
  | NOPROF -> Ocaml.VSum (("NOPROF", []))
and vof_imm_or_ximm v = Ocaml.vof_either vof_integer vof_ximm v
  
let vof_line =
  function
  | Pseudo v1 ->
      let v1 = vof_pseudo_instr v1 in Ocaml.VSum (("Pseudo", [ v1 ]))
  | Instr ((v1, v2)) ->
      let v1 = vof_instr v1
      and v2 = vof_condition v2
      in Ocaml.VSum (("Instr", [ v1; v2 ]))
  | LabelDef v1 -> let v1 = vof_label v1 in Ocaml.VSum (("LabelDef", [ v1 ]))
  | LineDirective ((v1, v2)) ->
      let v1 = Ocaml.vof_int v1
      and v2 = vof_filename v2
      in Ocaml.VSum (("LineDirective", [ v1; v2 ]))
  
let vof_program v =
  Ocaml.vof_list
    (fun (v1, v2) ->
       let v1 = vof_line v1 and v2 = vof_pos v2 in Ocaml.VTuple [ v1; v2 ])
    v
