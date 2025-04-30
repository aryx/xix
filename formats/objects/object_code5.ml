(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)

(* todo: use absolute path? realpath? *)
type object_code = 
  Ast_asm5.program * Location_cpp.location_history list

(* less: could be sha1 of ast_asm5.ml for even safer marshalling *)
let version = 6

(* can normalize before? or check every invariants? *)
let save obj file =
  let file = Fpath.v file in
  file |> UChan.with_open_out (fun (chan : Chan.o) ->
    output_value chan.oc (version, obj)
  )

(* for safer marshalling *)
exception WrongVersion

let load file =
  let file = Fpath.v file in
  file |> UChan.with_open_in (fun (chan : Chan.i) ->
    let (ver, obj) = input_value chan.ic in
    if ver <> version
    then raise WrongVersion
    else obj
  )
