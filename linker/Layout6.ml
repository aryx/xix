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

module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* claude: much simpler than Layout5.ml/Layout7.ml -- no literal pool at
 * all on this arch (see Codegen6.ml's own prelude for why: LEAQ's
 * absolute address and any move-immediate this checkpoint supports are
 * both encoded inline in the instruction stream, no separate pool
 * needed), and every instruction's SIZE (as opposed to its final byte
 * *values*, which do need real_pc) is already fully determined by its
 * own static AST shape (see Codegen6.ml's Call case comment for why
 * that holds even for CALL's rel32, whose size -- always exactly 5
 * bytes for a direct near call -- never depends on the actual
 * displacement). So this is just a a plain pc-accumulation walk. *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc)
  (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  let autosize = ref 0 in

  cg |> T.iter (fun n ->
    n.T.real_pc <- !pc;
    let size = Codegen6.size_of_instruction
        Codegen.{syms = symbols2; autosize = !autosize} n
    in
    (match n.T.instr with
    | T.TEXT (global, _, size) ->
        autosize := size;
        Hashtbl.add symbols2 (T.symbol_of_global global) (T.SText2 !pc);
    | _ -> ()
    );
    pc := !pc + size;
  );

  if !Flags.debug_layout then begin
    cg |> T.iter (fun (n : Ast_asm6.instr T.node) ->
      Logs.app (fun m -> m  "0x%x: %s" n.T.real_pc (Types6.show_instr n.T.instr));
      n.T.branch |> Option.iter (fun (n : Ast_asm6.instr T.node) ->
        Logs.app (fun m -> m " -> branch: 0x%x" n.T.real_pc)
      )
    );
  end;

  (* claude: no 8-byte rounding here (unlike Layout5/7.ml's `Int_.rnd
   * !pc 8`) -- that rounding pads a text segment up to its literal
   * pool's own alignment need, which doesn't exist on this arch. *)
  let final_text = !pc in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);

  symbols2, cg, textsize
