open Common

let is_builtin s =
  List.mem s ["cd"; "exit"]

let dispatch s args =
  raise Todo
