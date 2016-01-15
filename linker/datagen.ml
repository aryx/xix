(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5
module T = Types
module T5 = Types5

let gen symbols2 init_data sizes ds =
  let arr = Array.create sizes.T.data_size (Char.chr 0) in

  ds |> List.iter (fun d ->
    let T5.DATA (ent, offset2, size_slice, v) = d in
    let info = Hashtbl.find symbols2 (T5.symbol_of_entity ent) in
    match info with
    | T.SData2 offset ->
        let base = offset + offset2 in
        (match v with
        | Left i -> raise Todo
        | Right (String s) -> 
            for i = 0 to size_slice -1 do arr.(base + i) <- s.[i] done
        | Right (Address ent2) ->
            let info2 = Hashtbl.find symbols2 (T5.symbol_of_entity ent2) in
            let _i = 
              match info2 with
              | T.SText2 real_pc -> 
                  real_pc
              | T.SData2 offset | T.SBss2 offset -> 
                  init_data + offset
            in
            raise Todo
        )
    | T.SBss2 _ -> raise (Impossible "layout_data missed a DATA")
    | T.SText2 _ -> raise (Impossible "layout_data did this check")
  );
  arr



