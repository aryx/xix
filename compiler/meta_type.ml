(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_vof.cmo  pr_o.cmo /tmp/xxx.ml  *)

open Type_

module Ocaml = OCaml

let vof_blockid v = Ocaml.vof_int v
  
(* pad: modif vof_sign to be more compact *)
let vof_sign =
  function
  | Signed -> Ocaml.VSum (("S", []))
  | Unsigned -> Ocaml.VSum (("U", []))

let vof_fullname (v1, v2) =
  let v1 = Ocaml.vof_string v1 and v2 = vof_blockid v2 in 
  Ocaml.VTuple [ v1; v2 ]

let vof_struct_kind =
  function
  | Struct -> Ocaml.VSum (("Struct", []))
  | Union -> Ocaml.VSum (("Union", []))

  
let rec vof_t =
  function
  | Void -> Ocaml.VSum (("Void", []))
  | I v1 -> let v1 = vof_integer_type v1 in Ocaml.VSum (("I", [ v1 ]))
  | F v1 -> let v1 = vof_float_type v1 in Ocaml.VSum (("F", [ v1 ]))
  | Pointer v1 -> let v1 = vof_t v1 in Ocaml.VSum (("Pointer", [ v1 ]))
  | Array ((v1, v2)) ->
      let v1 = Ocaml.vof_option Ocaml.vof_int v1
      and v2 = vof_t v2
      in Ocaml.VSum (("Array", [ v1; v2 ]))
  | Func ((v1, v2, v3)) ->
      let v1 = vof_t v1
      and v2 = Ocaml.vof_list vof_t v2
      and v3 = Ocaml.vof_bool v3
      in Ocaml.VSum (("Func", [ v1; v2; v3 ]))
  | StructName ((v1, v2)) ->
      let v1 = vof_struct_kind v1
      and v2 = vof_fullname v2
      in Ocaml.VSum (("StructName", [ v1; v2 ]))

and vof_integer_type (v1, v2) =
  let v1 = vof_integer_kind v1
  and v2 = vof_sign v2
  in Ocaml.VTuple [ v1; v2 ]
and vof_integer_kind =
  function
  | Char -> Ocaml.VSum (("Char", []))
  | Short -> Ocaml.VSum (("Short", []))
  | Int -> Ocaml.VSum (("Int", []))
  | Long -> Ocaml.VSum (("Long", []))
  | VLong -> Ocaml.VSum (("VLong", []))

and vof_float_type =
  function
  | Float -> Ocaml.VSum (("Float", []))
  | Double -> Ocaml.VSum (("Double", []))

  
let vof_qualifier =
  function
  | Volatile -> Ocaml.VSum (("Volatile", []))
  | Const -> Ocaml.VSum (("Const", []))
  
let vof_structdef v1 =
  Ocaml.vof_list
    (fun (v1, v2) ->
      let v1 = Ocaml.vof_string v1
      and v2 = vof_t v2
      in Ocaml.VTuple [ v1; v2 ])
    v1

