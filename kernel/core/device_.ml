open Types

type devid = Types.devid

type t = {
  devcode: rune;
  name: string;

  (* methods *)

  reset: unit -> unit;
  (* less: init: unit -> unit; shutdown: unit -> unit *)
  
  open_: Chan_.t -> Chan_.open_mode -> Chan_.t;
  close: Chan_.t -> unit;

  read: Chan_.t -> user_addr * int -> int64 -> int ;
  write: Chan_.t -> user_addr * int -> int64 -> int;
  (* less: bread, bwrite *)

  (* todo:
  walk: 
  *)
  create: Chan_.t -> string -> perm -> unit; (* less: open_mode? *)
  remove: Chan_.t -> unit;

  (* less:
   stat, fwstat
  *)

  attach: string -> Chan_.t;

  (* less: power
  *)
}
