(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)

(* todo: use absolute path? realpath? *)
type object_code = 
  Ast_asm5.program * Common.filename (* src file *)

(* less: could be sha1 of ast_asm5.ml for even safer marshalling *)
let version = 5

(* can normalize before? or check every invariants? *)
let save obj file =
  file |> Common.with_file_out (fun chan ->
    output_value chan (version, obj)
  )

(* for safer marshalling *)
exception WrongVersion

let load file =
  file |> Common.with_file_in (fun chan ->
    let (ver, obj) = input_value chan in
    if ver <> version
    then raise WrongVersion
    else obj
  )
