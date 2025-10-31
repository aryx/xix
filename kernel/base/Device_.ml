open Common

type devid = Types.devid

type seek = Seek of int64

type t = {
  devcode: Types.rune;
  name: string;

  (* methods *)

  reset: unit -> unit;
  (* less: init: unit -> unit; shutdown: unit -> unit *)
  
  open_: Chan_.t -> Chan_.open_mode -> Chan_.t;
  close: Chan_.t -> unit;

  (* todo: should be Chan_.t -> user_addr * int -> int64 -> int;?
   * or virt_addr?
   *)
  read: Chan_.t -> string (* buffer *) -> int (* nb bytes *) -> seek -> int;
  write: Chan_.t -> string -> int -> seek -> int;
  (* less: bread, bwrite *)

  (* todo:
  walk: 
  *)
  create: Chan_.t -> string -> Types.perm -> unit; (* less: open_mode? *)
  remove: Chan_.t -> unit;

  (* less:
   stat, fwstat
  *)

  attach: string -> Chan_.t;

  (* less: power
  *)
}
