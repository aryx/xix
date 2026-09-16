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
  (* claude: pc of the earliest still-pending pool entry's own
   * referencing instruction, i.e. goken's real `pool.start`
   * (linkers/5l/layout.c's checkpool()/addpool()) -- used below to
   * proactively flush before an LDR's 12-bit PC-relative field would
   * overflow. Meaningless while literal_pools is empty. *)
  let pool_start_pc = ref 0 in

  (* claude: splice the pending pool (in FIFO order -- literal_pools
   * is a stack, LIFO, so without the reversal below the pool would
   * come out in reverse insertion order, a real, latent bug: every
   * earlier fixture only ever had one pending entry per flush, so
   * order never mattered until a fixture with 2+ simultaneous
   * entries, e.g. tests/linker/arm_diff/longoff_arm.s, exposed it;
   * goken's addpool()/flushpool() walk the pool in insertion order)
   * into the code stream right after `n`, ahead of whatever `n.next`
   * already was -- "extend cg, and so the cg |> T.iter, on the fly!"
   * (T.iter reads n.next *after* calling us on n, so it sees this
   * mutation and walks straight into the newly-spliced nodes). If
   * `guard` is given, it's inserted between `n` and the pool (a
   * branch jumping around the pool data -- see its own call sites
   * below for when one is/isn't needed); either way, the very last
   * spliced node's `.next` is reconnected to `n`'s *original*
   * `.next`, so the rest of the program is never lost. *)
  (* claude: recent OCaml accepts the optional-argument binder
   * `?guard` in a function definition -- ocaml-light's parser has no
   * support for optional arguments at all (`?label`/`?label:pattern`
   * both fail to parse), so `guard` is a plain, explicit
   * `... option` parameter instead; every call site below now passes
   * None/Some explicitly rather than omitting the argument. *)
  let flush_pool (guard : Ast_asm5.instr_with_cond Types.node option)
      (n : Ast_asm5.instr_with_cond Types.node) =
    let tail = n.next in
    let rec aux (prev : Ast_asm5.instr_with_cond Types.node) xs =
      match xs with
      | [] -> prev.next <- tail
      | x :: xs -> prev.next <- Some x; aux x xs
    in
    (match guard with
    | Some skip_branch ->
        n.next <- Some skip_branch;
        aux skip_branch (List.rev !literal_pools)
    | None ->
        (match List.rev !literal_pools with
        | [] -> ()
        | first :: rest -> n.next <- Some first; aux first rest)
    );
    literal_pools := []
  in

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
     *    program) -- see the flush logic below, after `pc` is
     *    updated for this node.
     *)
    let is_lpool_point = ref false in
    poolopt |> Option.iter (fun pool ->
      match pool with
      | Codegen5.LPOOL -> is_lpool_point := true
      | Codegen5.PoolOperand imm_or_ximm ->
          let instr = T.WORD imm_or_ximm in
          (* less: check if already present in literal_pools *)
          let node = Types.{ instr = instr; next = None; branch = None;
                             real_pc = -1;
                             n_loc = n.n_loc } in
          if node.branch <> None
          then raise (Impossible "attaching literal to branching instruction");

          if !literal_pools = [] then pool_start_pc := !pc;
          n.branch <- Some node;
          literal_pools |> Stack_.push node;
          Logs.debug (fun m -> m "added literal pool entry at pc=%d (pool now has %d entr%s)"
            !pc (List.length !literal_pools)
            (if List.length !literal_pools = 1 then "y" else "ies"));

    );
    pc := !pc + size;

    if !is_lpool_point && !literal_pools <> []
       && !pc + List.length !literal_pools * 4 - !pool_start_pc >= 2048
    then begin
      (* claude: goken's `case LPOOL: if(cond==ALWAYS) flushpool(p,
       * false)` -- `n` is already an unconditional control transfer
       * (Codegen5.ml sets `x = Some LPOOL` for *both* forms of `B`,
       * absolute-target and indirect/register-target alike -- e.g. a
       * leaf procedure's `RET` expands to `B(R14)`, still a real
       * unconditional branch -- matching goken's own optab.c, which
       * puts the LPOOL flag on both its case-5 and case-6 `AB` rows),
       * so nothing can ever fall through to whatever comes right
       * after it in the linear instruction stream -- no guard branch
       * needed, just splice the pool in directly (goken's own
       * `flushpool(p, skip=false)`). The `>= 2048` guard mirrors that
       * function's own early-return ("not worth it yet") -- initially
       * skipped here as a pure size trade-off, but it turned out to
       * have a real, observable byte-level effect (tests/linker/
       * arm_diff/call.s and kitchen_sink.s regressed without it: both
       * have a leaf RET pending a pool entry well under 2048 bytes
       * in, and goken's real 5l leaves it pending until the true
       * end-of-program flush in that case, not here). *)
      Logs.debug (fun m -> m "flushing %d pooled literal(s) at an unconditional branch (pc=%d)"
        (List.length !literal_pools) !pc);
      flush_pool None n
    end else if !literal_pools <> [] &&
                (n.next = None || !pc - !pool_start_pc >= 4000)
    then begin
      (* claude: goken's checkpool(): flush (WITH a guard branch this
       * time, since we can't prove nothing falls through here) once
       * we reach the true end of the linked program (n.next = None
       * -- this loop walks the whole concatenated code of every
       * object file, not just one; goken's `if(p->link==P)
       * flushpool(p,true)`), OR proactively, once the oldest pending
       * entry's own reference is getting close to the limit of an
       * LDR's 12-bit PC-relative field (goken's real check:
       * `pool.size>=0xffc || immaddr(...)==0` -- approximated here as
       * a flat "4000 bytes since the oldest entry was queued", a
       * safe conservative margin under the real 4095-byte limit,
       * chosen for simplicity over exactly replicating goken's
       * formula; this means large multi-file programs won't
       * necessarily flush at the exact same point goken would, but
       * every existing small fixture is far below this threshold, so
       * their behavior -- and byte output -- is unchanged).
       *
       * Why a branch is needed at all: without it, if execution ever
       * fell through past this point, the CPU would start decoding
       * the pool's raw WORD data as if it were more instructions --
       * garbage/undefined behavior.
       *
       * The *target* of that guard branch differs by trigger: at the
       * true end of the program there is no real "next instruction"
       * to jump to (goken's C code sets the branch's target to
       * `p->link`, NULL here; the observed, reproduced-below effect
       * is a branch that targets *itself*, an infinite loop -- dead
       * code in every test we have, since real programs end via an
       * exit syscall and never reach it, but goken emits it
       * unconditionally, so we match it byte-for-byte). Flushing
       * proactively mid-program, though, has a real `n.next` to jump
       * to -- the rest of the program must not be lost, so the guard
       * branch targets that instead of looping. *)
      let skip_branch : Ast_asm5.instr_with_cond Types.node =
        Types.{ instr = T.I (Ast_asm5.B (ref (A.Absolute 0)), Ast_asm5.AL);
                 next = None; branch = None; real_pc = -1; n_loc = n.n_loc }
      in
      (match n.next with
      | None -> skip_branch.branch <- Some skip_branch
      | Some real_next -> skip_branch.branch <- Some real_next
      );
      Logs.debug (fun m -> m "flushing %d pooled literal(s) %s (pc=%d)"
        (List.length !literal_pools)
        (if n.next = None then "at end of program" else "proactively (pool getting too far)")
        !pc);
      flush_pool (Some skip_branch) n
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
