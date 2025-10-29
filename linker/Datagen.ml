(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let gen (symbols2 : T.symbol_table2) (init_data : T.addr) (sizes : Exec_file.sections_size) (ds : T.data list) : T.byte array =
  let arr = Array.make sizes.data_size (Char.chr 0) in

  ds |> List.iter (fun d ->
    let T.DATA (global, offset2, size_slice, v) = d in
    let info = Hashtbl.find symbols2 (T.symbol_of_global global) in
    match info with
    | T.SData2 (offset, T.Data) ->
        let base = offset + offset2 in
        (match v with
        | A.Int _ | A.Float _ -> raise Todo
        | A.String s -> 
            for i = 0 to size_slice -1 do 
              arr.(base + i) <- s.[i] 
            done
        | A.Address (A.Global (global2,_offsetTODO)) ->
            let info2 = Hashtbl.find symbols2 (T.symbol_of_global global2) in
            let _i = 
              match info2 with
              | T.SText2 real_pc -> real_pc
              | T.SData2 (offset, _kind) -> init_data + offset
            in
            raise Todo
        | (A.Address (A.Local _ | A.Param _)) -> raise Todo
        )
    | T.SData2 (_, T.Bss) -> raise (Impossible "layout_data missed a DATA")
    | T.SText2 _ -> raise (Impossible "layout_data did this check")
  );
  arr
