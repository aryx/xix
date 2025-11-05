(*s: Layout5.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module T5 = Types5
module A = Ast_asm

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Layout5.xdefine]] *)
let xdefine (h2 : T.symbol_table2) (h : T.symbol_table) (symb : T.symbol) (v : T.section2) =
  (* stricter: we do not accept previous def of special symbols *)
  if Hashtbl.mem h symb || Hashtbl.mem h2 symb
  then failwith (spf "special symbol %s is already defined" (fst symb));

  Hashtbl.add h2 symb v
(*e: function [[Layout5.xdefine]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
(*s: function [[Layout5.layout_data]] *)
let layout_data (symbols : T.symbol_table) (ds : T.data list) : T.symbol_table2 * (int * int)
  =
  let h2 = Hashtbl.create 101 in

  (* a set *)
  let is_data = Hashtbl.create 101 in

  (* step0: identify Data vs Bss (and sanity check DATA instructions) *)
  ds |> List.iter (function
    | T.DATA (global, offset, size_slice, _v) ->
        (* sanity checks *)
        (match (T.lookup_global global symbols).T.section with
        | T.SData size ->
            if offset + size_slice > size
            then failwith (spf "initialize bounds (%d): %s" size
                             (A.s_of_global global))
        | T.SText _ -> failwith (spf "initialize TEXT, not a GLOBL for %s"
                                   (A.s_of_global global))
        | T.SXref -> raise (Impossible "SXRef detected by Check.check")
        );
        (* use replace cos can have multiple DATA for the same GLOBL *)
        Hashtbl.replace is_data (T.symbol_of_global global) true
  );

  (* step1: sanity check sizes and align *)
  symbols |> Hashtbl.iter (fun (s, _) v ->
    match v.T.section with
    (* less: do the small segment optimisation *)
    | T.SData size ->
        if size <= 0
        then failwith (spf "%s: no size" s);

        if size mod 4 <> 0
        then v.T.section <- T.SData (Int_.rnd size 4)
    | _ -> ()
  );

  let orig = ref 0 in

  (* step2: layout Data section *)
  symbols |> Hashtbl.iter (fun symb v ->
    match v.T.section with
    | T.SData size when Hashtbl.mem is_data symb ->
        Hashtbl.add h2 symb (T.SData2 (!orig, T.Data));
        orig := !orig + size;
    | _ -> ()
  );
  orig := Int_.rnd !orig 8;
  let data_size = !orig in

  (* step3: layout Bss section *)
  symbols |> Hashtbl.iter (fun symb v ->
    match v.T.section with
    | T.SData size when not (Hashtbl.mem is_data symb) ->
        Hashtbl.add h2 symb (T.SData2(!orig, T.Bss));
        orig := !orig + size;
    | _ -> ()
  );
  orig := Int_.rnd !orig 8;
  let bss_size = !orig - data_size in

  (* define special symbols *)
  xdefine h2 symbols ("bdata"  , T.Public) (T.SData2 (0, T.Data));
  xdefine h2 symbols ("edata"  , T.Public) (T.SData2 (data_size, T.Data));
  xdefine h2 symbols ("end"    , T.Public) (T.SData2 (data_size + bss_size, T.Data));
  xdefine h2 symbols ("setR12" , T.Public) (T.SData2 (0, T.Data));
  (* This is incorrect but it will be corrected later. This has
   * no consequence on the size of the code computed in layout_text
   * because address resolution for procedures always use a literal
   * pool.
   *)
  xdefine h2 symbols ("etext"  , T.Public) (T.SText2 0);

  h2, (data_size, bss_size)
(*e: function [[Layout5.layout_data]] *)

(*s: function [[Layout5.layout_text]] *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc) (cg : T5.code_graph) : T.symbol_table2 * T5.code_graph * int =

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
      Codegen5.size_of_instruction symbols2 !autosize n 
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
      Logs.app (fun m -> m  "%d: %s" n.real_pc (T5.show_instr n.instr));
      n.branch |> Option.iter (fun (n : Ast_asm5.instr_with_cond Types.node) -> 
        Logs.app (fun m -> m " -> branch: %d" n.real_pc)
      )
    );
  end;

  let final_text = Int_.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);
  
  symbols2, cg, textsize
(*e: function [[Layout5.layout_text]] *)
(*e: Layout5.ml *)
