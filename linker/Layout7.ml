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

(* claude: literal-pool splicing, ported from Layout5.ml (ARM32's own
 * mechanism, itself ported from goken's 5l/layout.c checkpool()/
 * flushpool()/addpool()) -- see Codegen7.ml's `pool` type/
 * gload_from_pool comment for why ARM64 needs one at all (confirmed
 * empirically: any "MOV $con,R" too big for a direct MOVZ/MOVN, and
 * *every* "MOV $sym(SB),R" address-of-global, go through goken's own
 * literal pool on this arch). Only the "flush at the true end of the
 * program" trigger is implemented, same scope as Layout5.ml's own
 * still-open mid-function-flush gap -- pools must stay small for now.
 * No literal-pool-value-deduplication either (same already-known gap
 * as ARM32's, see arm_port.md's "Open issues"). *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc)
  (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  let autosize = ref 0 in
  let literal_pools = ref [] in

  cg |> T.iter (fun n ->
    n.real_pc <- !pc;
    let size, poolopt =
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

    poolopt |> Option.iter (fun pool ->
      match pool with
      | Codegen7.PoolOperand imm_or_ximm ->
          (* claude: every pool entry here is a genuine 8-byte (64-bit)
           * value -- the load instruction that reads it is always a
           * 64-bit LDR (goken's own AMOV, never AMOVW, routes through
           * this mechanism, confirmed empirically -- see Codegen7.ml's
           * `pool` comment). The shared `Codegen.default_rules`'s WORD
           * case only ever emits a single 4-byte word (the low 32
           * bits, for `Int`, or the whole resolved address, for
           * `Address` -- which in this harness's address range always
           * fits in 32 bits anyway), so a second, explicit high-word
           * node is spliced right after it here. This also matters for
           * *alignment* when more than one pool entry exists in the
           * same flush (see the pre-pad comment below): without it,
           * a second entry would land only 4 bytes after the first,
           * not the 8-byte-aligned slot goken itself always gives each
           * entry (confirmed empirically -- goken never deduplicates
           * even byte-identical pool values into one slot, so a
           * fixture with two "MOV $sym(SB),R"-shaped loads of the
           * exact same address gets two distinct, separately-aligned
           * 8-byte entries). *)
          let low_node = Types.{ instr = T.WORD imm_or_ximm; next = None;
                                  branch = None; real_pc = -1; n_loc = n.n_loc } in
          let high_value = (match imm_or_ximm with
            | Ast_asm.Int i -> (i asr 32) land 0xffffffff
            | Ast_asm.Address _ -> 0
            (* claude: the high 32 bits of the same raw IEEE754 bit
             * pattern whose low 32 bits Codegen.ml's own WORD case
             * just emitted -- must match exactly, or a double literal
             * round-trips as a different value. *)
            | Ast_asm.Float f ->
                let bits = Int64.bits_of_float f in
                Int64.to_int (Int64.shift_right_logical bits 32) land 0xffffffff
            | Ast_asm.String _ -> 0
          ) in
          let high_node = Types.{ instr = T.WORD (Ast_asm.Int high_value);
                                   next = None; branch = None; real_pc = -1;
                                   n_loc = n.n_loc } in
          low_node.next <- Some high_node;
          if n.branch <> None
          then raise (Impossible "attaching literal to branching instruction");
          n.branch <- Some low_node;
          literal_pools |> Stack_.push low_node;
    );
    pc := !pc + size;

    (* claude: flush the pool at the true end of the (whole,
     * concatenated) program -- see Layout5.ml's own comment for the
     * full "why a guard branch is needed at all" reasoning; same
     * self-branch idiom here (B .), just an ARM64 `B` instruction
     * instead of ARM32's. *)
    if n.next = None && !literal_pools <> [] then begin
      let skip_branch : Ast_asm7.instr Types.node =
        Types.{ instr = T.I (Ast_asm7.B (ref (A.Absolute 0)));
                 next = None; branch = None; real_pc = -1; n_loc = n.n_loc }
      in
      skip_branch.branch <- Some skip_branch;

      (* claude: each pool entry pushed above is a 2-node (low,high)
       * pair already chained via its own `.next` -- walk to the pair's
       * tail before linking the next pair, so the whole pool threads
       * as one flat sequence: low1,high1,low2,high2,... *)
      let rec aux (prev : Ast_asm7.instr Types.node) xs =
        match xs with
        | [] -> ()
        | x::xs ->
            prev.next <- Some x;
            let tail = match x.next with Some h -> h | None -> x in
            aux tail xs
      in
      n.next <- Some skip_branch;
      (* claude: real AArch64 gotcha, caught by a fixture with an odd
       * number of leading instructions (address-of-global with an
       * extra SB-relative load ahead of it): goken 8-byte-aligns the
       * literal pool's start, inserting a single 4-byte zero-word pad
       * right after the guard branch when the natural position isn't
       * already aligned -- confirmed directly against goken (a plain
       * WORD-sized filler entry, not folded into any real
       * instruction). Every "MOV $con,R"/"MOV $sym(SB),R" pool entry
       * here is conceptually 8 bytes (a 64-bit value), even though
       * it's built from a single 4-byte T.WORD node -- the *other*
       * 4 bytes come from ordinary end-of-.text rounding (see the
       * final `Int_.rnd` below) when the pool is the very last thing,
       * but that trick alone doesn't guarantee the pool *starts*
       * aligned when something real follows it in a longer program,
       * hence this explicit pre-pad. *)
      let after_guard_pc = !pc + 4 in
      let tail =
        if after_guard_pc mod 8 <> 0 then begin
          let pad = Types.{ instr = T.WORD (Ast_asm.Int 0);
                             next = None; branch = None; real_pc = -1;
                             n_loc = n.n_loc } in
          skip_branch.next <- Some pad;
          pad
        end else skip_branch
      in
      (* literal_pools is a stack (LIFO push) -- reverse to FIFO
       * (insertion) order, same fix ARM32's own port needed (see
       * Layout5.ml's comment) *)
      aux tail (List.rev !literal_pools);
      literal_pools := [];
    end;
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
