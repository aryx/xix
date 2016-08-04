
val execsh: 
  Shellenv.t -> string (* sh arguments *) -> Ast.recipe (* sh stdin *) ->
  int (* pid *)
