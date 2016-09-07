open Common

let is_builtin s =
  List.mem s ["cd"; "exit"]

