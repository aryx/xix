open Common
open Types

let ibrk addr_opt section =
  raise Todo

let syscall_brk addr_opt =
  ibrk addr_opt Proc_.SBss

