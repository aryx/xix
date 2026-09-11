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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* claude: RISC-V never needs a literal pool the way ARM (Layout5.ml)
 * does: a 32-bit constant too big for a 12-bit immediate is always
 * materialized inline at its use site via LUI (+ ADDI), not spliced
 * in later as a separate WORD elsewhere in the instruction stream --
 * see goken's asm.c cases 8/9/12/13/14/15/16/20. So unlike
 * Layout5.ml/Layoutv.ml, there is no pool bookkeeping here at all;
 * this is a straight port of vl's simpler layout_text.
 *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc)
  (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  let autosize = ref 0 in

  cg |> T.iter (fun n ->
    n.real_pc <- !pc;
    let size =
      Codegeni.size_of_instruction
          Codegen.{syms = symbols2; autosize = !autosize} n
    in
    if size = 0
    then
      (match n.instr with
      | T.TEXT (global, _, size) ->
          autosize := size;
          Hashtbl.add symbols2 (T.symbol_of_global global) (T.SText2 !pc);
      | _ -> failwith (spf "zero-width instruction at %s" (T.s_of_loc n.n_loc))
      );
    pc := !pc + size;

  );
  if !Flags.debug_layout then begin
    cg |> T.iter (fun (n : Ast_asmi.instr Types.node) ->
      Logs.app (fun m -> m  "0x%x: %s" n.real_pc (Typesi.show_instr n.instr));
      n.branch |> Option.iter (fun (n : Ast_asmi.instr Types.node) ->
        Logs.app (fun m -> m " -> branch: 0x%x" n.real_pc)
      )
    );
  end;

  let final_text = Int_.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);

  symbols2, cg, textsize
