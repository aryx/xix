
val execsh: 
  Shellenv.t -> string list (* sh arguments *) -> string list (* sh stdin *) ->
  bool (* interactive *) ->
  int (* pid *)
