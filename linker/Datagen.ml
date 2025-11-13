(*s: Datagen.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fill_bytes_for_int (global : A.global) (arr : T.byte array) (base : int)
    (n : int) (bits: Arch.bits) (endian: Endian.t) :
    unit =

  let array_16, array_32 = Endian.array_functions_of_endian endian in

   (* TODO: if negative still need check range and convert to 
    * corresponding unsigned value with sign bits on? 
    *)
   match bits with
   | Arch.Arch8 when n >= 0 && n <= 0xff -> 
      arr.(base) <- (Char.chr n)
   | Arch.Arch16 when n >= 0 && n <= 0xffff -> 
      array_16 n |> Array.iteri (fun i el -> arr.(base + i) <- el)
   | Arch.Arch32 when n >= 0 && n <= 0xffffffff -> 
      array_32 n |> Array.iteri (fun i el -> arr.(base + i) <- el)
   | Arch.Arch64 -> raise Todo
   | _ -> 
      failwith (spf "int for %s < 0 or too big for its size"
                            (A.s_of_global global))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Datagen.gen]] *)
let gen (symbols2 : T.symbol_table2) (init_data : T.addr)
        (sizes : Exec_file.sections_size) (endian: Endian.t)
        (ds : T.data list) :
        T.byte array =
  let arr : T.byte array =
    Array.make sizes.data_size (Char.chr 0) in

  ds |> List.iter (fun d ->
      (* alt: have a Flags.debug_data *)
    Logs.debug (fun m -> m "Datagen.gen for %s" (T.show_data d));
      
    let T.DATA (global, offset2, size_slice, v) = d in
    let info = Hashtbl.find symbols2 (T.symbol_of_global global) in
    match info with
    | T.SData2 (offset, T.Data) ->
        let base = offset + offset2 in

        (match v with
        | A.Int n -> 
            (match size_slice with
            | 1 | 2 | 4 ->
               fill_bytes_for_int global arr base n 
                        (Arch.bits_of_intsize size_slice) endian
            | _ -> 
                failwith (spf "size for %s not in {1,2,4}"
                            (A.s_of_global global));
            )

        | A.Float _ -> failwith "TODO: Datagen.gen for Float"

        | A.String s ->
            if size_slice > 8 
            then failwith (spf "size for %s > 8" (A.s_of_global global));

            for i = 0 to size_slice -1 do 
              arr.(base + i) <- s.[i] 
            done

        | A.Address (A.Global (global2, offset_global)) ->
            (* TODO? always true? *)
            assert (offset_global = 0);
            let info2 = Hashtbl.find symbols2 (T.symbol_of_global global2) in
            let n = 
              match info2 with
              | T.SText2 real_pc -> real_pc
              | T.SData2 (offset, _kind) -> init_data + offset
            in
            (* TODO: what about 64 bits arch? *)
            fill_bytes_for_int global (* not global2*) arr base n 
                  Arch.Arch32 endian

        | (A.Address (A.Local _ | A.Param _)) ->
            raise (Impossible "address of local or param in DATA")
        )

    | T.SData2 (_, T.Bss) -> raise (Impossible "layout_data missed a DATA")
    | T.SText2 _ -> raise (Impossible "layout_data did this check")
  );

  (* TODO? -debug_data and output its content? *)
  arr
(*e: function [[Datagen.gen]] *)
(*e: Datagen.ml *)
