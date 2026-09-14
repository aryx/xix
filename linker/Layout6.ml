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
module A6 = Ast_asm6
module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* claude: no literal pool at all on this arch (see Codegen6.ml's own
 * prelude for why: LEAQ's absolute address and any move-immediate this
 * checkpoint supports are both encoded inline in the instruction
 * stream, no separate pool needed), so this is otherwise just a plain
 * pc-accumulation walk -- *except* for Jmp/Jcc's own short-vs-near
 * relaxation (see Ast_asm6.ml's own Jmp/Jcc comment for why real
 * amd64 needs it at all): unlike every other instruction here, a
 * Jmp/Jcc's SIZE genuinely depends on a distance that isn't known
 * until every node's real_pc is assigned, which itself depends on
 * every instruction's size -- a real chicken-and-egg problem. `relax`
 * below resolves it with a classic fixed-point: assume every Jmp/Jcc
 * is short, lay out the whole program, then check every still-short
 * one against its now-known (for this round) target distance, forcing
 * any that don't fit to the near form and re-laying-out if anything
 * changed. This always terminates: forcing a jump long only ever grows
 * an instruction's size, which can only ever push some OTHER jump's
 * resolved distance *further* away, never closer (real_pc values are
 * monotonically non-decreasing round over round), so once a jump is
 * found to fit, it stays fit forever -- bounded by the number of Jmp/
 * Jcc instructions in the program. *)
let relax (symbols2 : T.symbol_table2) (init_text : T.real_pc)
    (cg : A6.instr T.code_graph) : T.real_pc =
  let rec one_round () : T.real_pc =
    let pc = ref init_text in
    let autosize = ref 0 in
    cg |> T.iter (fun n ->
      n.T.real_pc <- !pc;
      let size = Codegen6.size_of_instruction
          Codegen.{syms = symbols2; autosize = !autosize} n
      in
      (match n.T.instr with
      | T.TEXT (global, _, size) ->
          autosize := size;
          Hashtbl.replace symbols2 (T.symbol_of_global global) (T.SText2 !pc);
      | _ -> ()
      );
      pc := !pc + size;
    );
    let final_pc = !pc in
    let changed = ref false in
    (* claude: extracts the ref's own current `bool` value via an
     * ordinary `!` dereference before branching (not a nested
     * `{contents = ...}` pattern on the instruction match below) --
     * see Codegen6.ml's own Jmp/Jcc comment for why. *)
    let check (long_ref : bool ref) (branch : 'a T.node option) (self_pc : T.real_pc) : unit =
      if not !long_ref then
        match branch with
        | None -> ()
        | Some ndst ->
            let rel = ndst.T.real_pc - (self_pc + 2) in
            if rel < -128 || rel > 127 then begin long_ref := true; changed := true end
    in
    cg |> T.iter (fun n ->
      match n.T.instr with
      | T.I (A6.Jmp (_, long_ref)) -> check long_ref n.T.branch n.T.real_pc
      | T.I (A6.Jcc (_, _, long_ref)) -> check long_ref n.T.branch n.T.real_pc
      | T.I _ | T.TEXT _ | T.WORD _ | T.Virt _ -> ()
    );
    if !changed then one_round () else final_pc
  in
  one_round ()

let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc)
  (cg : A6.instr T.code_graph) : T.symbol_table2 * A6.instr T.code_graph * int =

  let final_text = relax symbols2 init_text cg in

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
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);

  symbols2, cg, textsize
