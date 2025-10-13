(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm5
module T = Types
module T5 = Types5

let gen symbols2 init_data sizes ds =
  let arr = Array.make sizes.T.data_size (Char.chr 0) in

  ds |> List.iter (fun d ->
    let T5.DATA (global, offset2, size_slice, v) = d in
    let info = Hashtbl.find symbols2 (T5.symbol_of_global global) in
    match info with
    | T.SData2 offset ->
        let base = offset + offset2 in
        (match v with
        | Left _i -> raise Todo
        | Right (String s) -> 
            for i = 0 to size_slice -1 do 
              arr.(base + i) <- s.[i] 
            done
        | Right (Address (Global (global2,_offsetTODO))) ->
            let info2 = Hashtbl.find symbols2 (T5.symbol_of_global global2) in
            let _i = 
              match info2 with
              | T.SText2 real_pc -> 
                  real_pc
              (* ocaml-light: | T.SData2 offset | T.SBss2 offset ->  *)
              | T.SData2 offset -> init_data + offset
              | T.SBss2 offset -> init_data + offset
            in
            raise Todo
        | Right (Address (Local _ | Param _)) -> raise Todo
        )
    | T.SBss2 _ -> raise (Impossible "layout_data missed a DATA")
    | T.SText2 _ -> raise (Impossible "layout_data did this check")
  );
  arr



