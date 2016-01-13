(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5
module T = Types
module T5 = Types5

let xdefine h2 h symb v =
  (* stricter: we do not accept previous def of special symbols *)
  if Hashtbl.mem h symb || Hashtbl.mem h2 symb
  then failwith (spf "special symbol %s is already defined" (fst symb));

  Hashtbl.add h2 symb v



let layout_data symbols ds =
  let h2 = Hashtbl.create 101 in

  (* a set *)
  let is_data = Hashtbl.create 101 in

  (* step0: identify Data vs Bss (and sanity check DATA instructions) *)
  ds |> List.iter (function
    | T5.DATA (ent, offset, size_slice, _) ->
        (* sanity checks *)
        (match (T5.lookup_ent ent symbols).T.section with
        | T.SData size ->
            if offset + size_slice > size
            then failwith (spf "initialize bounds (%d): %s" size
                             (T5.s_of_ent ent))
        | T.SText _ -> failwith (spf "initialize TEXT, not data for %s"
                                   (T5.s_of_ent ent))
        | T.SXref -> raise (Impossible "SXRef detected by Check.check")
        );
        (* use replace cos can have multiple DATA for the same GLOBL *)
        Hashtbl.replace is_data (T5.symbol_of_entity ent) true
  );

  (* step1: sanity check sizes and align *)
  symbols |> Hashtbl.iter (fun (s, _) v ->
    match v.T.section with
    (* less: do the small segment thing *)
    | T.SData size ->
        if size <= 0
        then failwith (spf "%s: no size" s);
        if size mod 4 <> 0
        then v.T.section <- T.SData (Common.rnd size 4)
    | _ -> ()
  );

  let orig = ref 0 in
  (* step2: layout Data section *)
  symbols |> Hashtbl.iter (fun symb v ->
    match v.T.section with
    | T.SData size when Hashtbl.mem is_data symb ->
        Hashtbl.add h2 symb (T.SData2 !orig);
        orig := !orig + size;
    | _ -> ()
  );
  orig := Common.rnd !orig 8;
  let data_size = !orig in

  (* step3: layout Bss section *)
  symbols |> Hashtbl.iter (fun symb v ->
    match v.T.section with
    | T.SData size when not (Hashtbl.mem is_data symb) ->
        Hashtbl.add h2 symb (T.SBss2 !orig);
        orig := !orig + size;
    | _ -> ()
  );
  orig := Common.rnd !orig 8;
  let bss_size = !orig in

  (* define special symbols *)
  xdefine h2 symbols ("bdata", T.Public) (T.SData2 0);
  xdefine h2 symbols ("edata", T.Public) (T.SData2 data_size);
  xdefine h2 symbols ("end", T.Public) (T.SData2 (data_size + bss_size));
  xdefine h2 symbols ("setR12", T.Public) (T.SData2 0);
  (* this is incorrect but it will be corrected later *)
  xdefine h2 symbols ("etext", T.Public) (T.SText2 0);

  h2, (data_size, bss_size)




let layout_text symbols2 init_text cg =

  let pc = ref init_text in

  cg |> T5.iter (fun n ->
    n.T5.real_pc <- !pc;

    (* TODO: pool handling *)
    let size = Codegen5.size_of_instruction symbols2 n in
    (match n.T5.node with
    | T5.TEXT (ent, _, _) ->
        (* Useful for something except find pc of entry point?
         * Yes for getting the address of a procedure, e.g. in WORD $foo(SB)
         *)
        Hashtbl.add symbols2 (T5.symbol_of_entity ent) (T.SText2 !pc);
    | _ -> failwith (spf "zero-width instruction at %s" (T5.s_of_loc n.T5.loc))
    );
    (* TODO: pool handling *)
    pc := !pc + size
  );
  let final_text = Common.rnd !pc 8 in
  let textsize = final_text - init_text in
  Hashtbl.replace symbols2 ("etext", T.Public) (T.SText2 final_text);
  
  symbols2, cg, textsize

