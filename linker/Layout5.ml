(*s: Layout5.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module A = Ast_asm

(*s: function [[Layout5.xdefine]] *)
(*e: function [[Layout5.xdefine]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
(*s: function [[Layout5.layout_data]] *)
(*e: function [[Layout5.layout_data]] *)

(* TODO: seems reusable if pass Codegen5.size_of_instruction? move
 * to Latout.ml with layout_date?
 *)
(*s: function [[Layout5.layout_text]] *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc) (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  (* less: could be a None, to be more precise, to detect use of local/param
   * outside a procedure. But anyway at frontier of objects we
   * are considered in TEXT of preceding obj which does not make
   * much sense (we should do this kind of check in check.ml though).
   *)
  let autosize = ref 0 in
  let literal_pools = ref [] in

  cg |> T.iter (fun n ->
    n.real_pc <- !pc;

    let size, poolopt = 
      Codegen5.size_of_instruction 
          Codegen.{syms = symbols2; autosize = !autosize} n 
    in
    if size = 0
    then
      (match n.instr with
      | T.TEXT (global, _, size) ->
          (* remember that rewrite5 has adjusted autosize correctly *)
          autosize := size;
          (* Useful to find pc of entry point and to get the address of a
           * procedure, e.g. in WORD $foo(SB)
           *)
          Hashtbl.add symbols2 (T.symbol_of_global global) (T.SText2 !pc);
      | _ -> failwith (spf "zero-width instruction at %s" 
                         (T.s_of_loc n.n_loc))
      );
    (* claude: this is the "literal pool" mechanism, ported from
     * checkpool()/flushpool()/addpool() in goken's 5l/layout.c. The
     * problem it solves: ARM instructions are 32 bits, so most
     * opcodes can't embed a full 32-bit constant (e.g. the address
     * of a global) as an immediate operand. The trick 5l/5a use is
     * to instead emit a `LDR Rt, [PC, #offset]` and stash the actual
     * 32-bit value as a plain WORD *inside the instruction stream
     * itself*, a bit further along, so it can be loaded PC-relative
     * -- that stashed WORD is one entry in the "literal pool".
     *
     * Two things below build on that:
     *  - Codegen5.PoolOperand: this instruction (a MOVW of an address
     *    too big to fit as an immediate) needs a pool entry. We
     *    create a WORD node to hold the value and attach it via
     *    n.branch (the same field a real branch would use to point
     *    at its target -- see gload_from_pool/gbranch_static in
     *    Codegen5.ml, which read it back through that same field);
     *    the node itself is queued in literal_pools, to be spliced
     *    into the code stream once we decide to "flush" the pool.
     *  - Codegen5.LPOOL: this instruction is a natural place goken's
     *    checkpool() would consider flushing the pool (in 5l: any
     *    unconditional branch, or falling off the end of the whole
     *    program). We only implement the "falling off the end"
     *    trigger below (see the flush block); mid-function flushing
     *    (needed once a pool grows past ~4KB or an LDR offset would
     *    stop fitting in 12 bits) is not implemented, so pools must
     *    stay small for now -- fine for our current test corpus, but
     *    a real limitation to lift later.
     *)
    poolopt |> Option.iter (fun pool ->
      match pool with
      | Codegen5.LPOOL -> ()
      | Codegen5.PoolOperand imm_or_ximm ->
          let instr = T.WORD imm_or_ximm in
          (* less: check if already present in literal_pools *)
          let node = Types.{ instr = instr; next = None; branch = None;
                             real_pc = -1;
                             n_loc = n.n_loc } in
          if node.branch <> None
          then raise (Impossible "attaching literal to branching instruction");

          n.branch <- Some node;
          literal_pools |> Stack_.push node;
          Logs.debug (fun m -> m "added literal pool entry at pc=%d (pool now has %d entr%s)"
            !pc (List.length !literal_pools)
            (if List.length !literal_pools = 1 then "y" else "ies"));

    );
    pc := !pc + size;

    (* claude: flush the pool once we reach the true end of the
     * linked program (n.next = None -- this loop walks the whole
     * concatenated code of every object file, not just one). This
     * corresponds to goken's `if(p->link == P) flushpool(p, true)`
     * in checkpool(): the "true" argument means "insert a branch
     * that jumps *around* the pool before emitting it".
     *
     * Why a branch is needed at all: without it, if execution ever
     * fell through past the last real instruction, the CPU would
     * start decoding the pool's raw WORD data as if it were more
     * instructions -- garbage/undefined behavior. goken always adds
     * the guard branch here because, in general, it cannot prove
     * nothing falls through to this point (that's a whole-program
     * property, not something checkpool can see locally).
     *
     * In our specific case (true end of the *entire* program) there
     * is no "next instruction" to jump to -- there's nothing after
     * the pool at all. goken's C code sets the branch's target to
     * `p->link`, which is NULL here; the observed, reproduced-below
     * effect is a branch that targets *itself* (an infinite loop).
     * That's dead code in every test we have (real programs end via
     * an exit syscall and never reach it), but goken emits it
     * unconditionally, so we match it byte-for-byte.
     *
     * todo: the "complex condition" 5l also has (early flush when
     * possible out of offset range) still isn't implemented.
     *)
    if n.next = None && !literal_pools <> [] then begin
      Logs.debug (fun m -> m "flushing %d pooled literal(s) at end of program (pc=%d)"
        (List.length !literal_pools) !pc);
      (* a self-branch, "B .": target = its own address, so give it
       * branch = Some itself. See gbranch_static in Codegen5.ml,
       * which computes the jump offset purely from node.branch's
       * real_pc vs its own -- offset ends up (0 - 8) = -8 bytes,
       * i.e. "jump back to right where you are", the standard
       * ARM idiom for an unreachable trap/infinite loop.
       *)
      let skip_branch : Ast_asm5.instr_with_cond Types.node =
        Types.{ instr = T.I (Ast_asm5.B (ref (A.Absolute 0)), Ast_asm5.AL);
                 next = None; branch = None; real_pc = -1; n_loc = n.n_loc }
      in
      skip_branch.branch <- Some skip_branch;

      (* extend cg, and so the cg |> T5.iter, on the fly! *)
      let rec aux (prev : Ast_asm5.instr_with_cond Types.node) xs =
        match xs with
        | [] -> ()
        | x::xs ->
            (* cg grows *)
            prev.next <- Some x;
            aux x xs
      in
      n.next <- Some skip_branch;
      aux skip_branch !literal_pools;
      literal_pools := [];
    end;

  );
  if !Flags.debug_layout then begin
    cg |> T.iter (fun (n : Ast_asm5.instr_with_cond Types.node) ->
      Logs.app (fun m -> m  "0x%x: %s" n.real_pc (Types5.show_instr n.instr));
      n.branch |> Option.iter (fun (n : Ast_asm5.instr_with_cond Types.node) -> 
        Logs.app (fun m -> m " -> branch: 0x%x" n.real_pc)
      )
    );
  end;

  let final_text = Int_.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);
  
  symbols2, cg, textsize
(*e: function [[Layout5.layout_text]] *)
(*e: Layout5.ml *)
