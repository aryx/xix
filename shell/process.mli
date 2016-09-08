
type waitfor_result =
  | WaitforInterrupted
  | WaitforFound
  | WaitforNotfound

val waitfor: int -> waitfor_result


val s_of_unix_error: Unix.error -> string -> string -> string
