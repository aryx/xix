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
    poolopt |> Option.iter (fun pool ->
      match pool with
      | Codegen5.LPOOL -> Logs.err (fun m -> m "TODO: LPOOL")
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
          
    );
    pc := !pc + size;

    (* flush pool *)
    (* todo: complex condition when possible out of offset range *)
    if n.next = None && !literal_pools <> [] then begin
      (* extend cg, and so the cg |> T5.iter, on the fly! *)
      let rec aux (prev : Ast_asm5.instr_with_cond Types.node) xs =
        match xs with
        | [] -> ()
        | x::xs ->
            (* cg grows *)
            prev.next <- Some x;
            aux x xs
      in
      aux n !literal_pools;
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
