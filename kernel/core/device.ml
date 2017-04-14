open Types

type devid = Types.devid

type t = {
  devcode: rune;
  name: string;

  (* methods *)

  reset: unit -> unit;
  (* less: init: unit -> unit; shutdown: unit -> unit *)
  
  open_: Chan.t -> Chan.open_mode -> Chan.t;
  close: Chan.t -> unit;

  read: Chan.t -> user_addr * int -> int64 -> int ;
  write: Chan.t -> user_addr * int -> int64 -> int;
  (* less: bread, bwrite *)

  (* todo:
  walk: 
  *)
  create: Chan.t -> string -> perm -> unit; (* less: open_mode? *)
  remove: Chan.t -> unit;

  (* less:
   stat, fwstat
  *)

  attach: string -> Chan.t;

  (* less: power
  *)
}
