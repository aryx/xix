(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module A = Ast_asm

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let xdefine (h2 : T.symbol_table2) (h : T.symbol_table) (symb : T.symbol) (v : T.section2) : unit =
  (* stricter: we do not accept previous def of special symbols *)
  if Hashtbl.mem h2 symb
  then failwith (spf "special symbol %s is already defined" (fst symb));

  (* also add to h so that Check.check will not yell for xdefine'd
   * constants such as setR30, setR12, etext, etc
   *)
  let v1 = T.lookup symb None h in
  (match v1.section with
  | T.SXref -> 
        (* the actual section is not important; this is just for Check.check()
         * to not yell for special symbols.
         * alt: could derive the section from section2
         *)
        v1.section <- T.SData 0
  | T.SText _ | T.SData _ -> 
     failwith (spf "special symbol %s is already defined" (fst symb))
  );

  Hashtbl.add h2 symb v

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
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

        (* TODO? ARM specific? *)
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
  (* TODO? ARM specific? *)
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
  xdefine h2 symbols ("end"    , T.Public) (T.SData2 (data_size + bss_size, T.Bss));
  (* This is incorrect but it will be corrected later. This has
   * no consequence on the size of the code computed in layout_text
   * because address resolution for procedures always use a literal
   * pool.
   *)
  xdefine h2 symbols ("etext"  , T.Public) (T.SText2 0);

  h2, (data_size, bss_size)

