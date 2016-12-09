(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Arch
module T = Type

let rec width_of_type env t =
  match t with
  | T.Void -> 0
  | T.I (inttype, _sign) ->
    (match inttype with
    | T.Char -> 1
    | T.Short -> 2
    | T.Int -> 4
    | T.Long -> 4
    | T.VLong -> 8
    )
  | T.F T.Float -> 4
  | T.F T.Double -> 8
  | T.Pointer _ -> 4

  | T.Func _ -> raise (Impossible "width of Func")
  | T.Array (iopt, t) ->
    (match iopt with
    | None -> raise (Impossible "width of incomplete array")
    | Some i -> i * width_of_type env t
    )
  | T.StructName (su, fullname) ->
      let _flds = Hashtbl.find env.structs fullname in
      raise Todo

let arch = {
  width_of_type = width_of_type;
}
