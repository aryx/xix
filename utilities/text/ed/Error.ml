exception Error of string

let error s =
  raise (Error s)

let error_1 (out : out_channel) s =
  output_string out s
