(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module Tv = Typesv
module A = Ast_asm

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* TODO: factorize with Layout5.ml and move layout_text in Layout.ml? *)
let layout_text (symbols2 : T.symbol_table2) (init_text : T.real_pc) 
  (cg : 'a T.code_graph) : T.symbol_table2 * 'a T.code_graph * int =

  let pc : T.real_pc ref = ref init_text in
  let autosize = ref 0 in
  let literal_pools = ref [] in

  cg |> T.iter (fun n ->
    n.real_pc <- !pc;
    let size, poolopt = 
      Codegenv.size_of_instruction 
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
      | Codegen.LPOOL -> Logs.err (fun m -> m "TODO: LPOOL1")
      | Codegen.PoolOperand _imm_or_ximm -> Logs.err (fun m -> m "TODO: LPOOL2")
    );
    pc := !pc + size;

    (* flush pool *)
    if n.next = None && !literal_pools <> [] then begin
        Logs.err (fun m -> m "TODO: LPOOL3")
    end;

  );
  if !Flags.debug_layout then begin
    cg |> T.iter (fun (n : Ast_asmv.instr Types.node) ->
      Logs.app (fun m -> m  "0x%x: %s" n.real_pc (Tv.show_instr n.instr));
      n.branch |> Option.iter (fun (n : Ast_asmv.instr Types.node) -> 
        Logs.app (fun m -> m " -> branch: 0x%x" n.real_pc)
      )
    );
  end;

  let final_text = Int_.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);
  
  symbols2, cg, textsize
