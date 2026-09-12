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

(* claude: no literal pool yet (unlike Layout5.ml/Layoutv.ml) -- v1 only
 * covers instructions that never need one (register-register/small-
 * immediate arith, plain-register-base memory access, branches). goken's
 * *own* ARM64 large-constant/address-of-global mechanism does turn out to
 * need a real literal pool too (confirmed empirically, see Ast_asm7.ml's
 * prelude comment and Codegen7.ml's "not yet implemented" notes) -- that's
 * real follow-up work, not a design decision to avoid it forever, just
 * deferred past this first version. When it lands, port Layout5.ml's
 * pool-splicing shape (the "flush at end of program, self-branch guard"
 * mechanism), not reinvent it. *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc)
  (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  let autosize = ref 0 in

  cg |> T.iter (fun n ->
    n.real_pc <- !pc;
    let size =
      Codegen7.size_of_instruction
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
    cg |> T.iter (fun (n : Ast_asm7.instr Types.node) ->
      Logs.app (fun m -> m  "0x%x: %s" n.real_pc (Types7.show_instr n.instr));
      n.branch |> Option.iter (fun (n : Ast_asm7.instr Types.node) ->
        Logs.app (fun m -> m " -> branch: 0x%x" n.real_pc)
      )
    );
  end;

  let final_text = Int_.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);

  symbols2, cg, textsize
