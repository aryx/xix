(* Yoann Padioleau
 *
 * Copyright (C) 2015, 2016 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

type object_code = 
  Ast_asm5.program * Common.filename

(* less: could be sha1 of ast_asm5.ml for safe marshalling *)
let version = 1

(* can normalize before? *)
let save obj file =
  file |> Common.with_file_out (fun chan ->
    output_value chan (version, obj)
  )
