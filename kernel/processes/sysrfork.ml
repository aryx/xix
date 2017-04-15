open Common
open Types


let syscall_rfork flags =
  match flags with
  | Syscall.Fork (fork_flags, flags) ->
    raise Todo
  | Syscall.NoFork (flags) -> 
    raise Todo
