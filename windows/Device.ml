(*s: Device.ml *)
(* Copyright 2017-2026 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to build "virtual devices" (e.g., a virtual /dev/cons).
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(*s: type [[Device.t]] *)
type t = {
  (* ex: "winname", "cons" *)
  name: string;
  perm: Plan9.perm_property;

  (* called when a process is opening/closing the device *)
  open_: Window.t -> unit;
  close: Window.t -> unit;
  
  (* called when a process is reading/writing on the device.
   *
   * we need to thread 'read' (and 'write') because this operation may spend
   * some time waiting while receiving and sending information on channels.
   * 
   * Note that 'read' takes an offset (and count) because each device 
   * can honor or not the offset requirements. For instance,
   * /dev/winname does honor offset but /dev/mouse does not.
   *)
  read_threaded: int64 -> int -> Window.t -> string (* bytes *);
  write_threaded: int64 -> string (* bytes *) -> Window.t -> unit;
}
(*e: type [[Device.t]] *)
(*s: constant [[Device.default]] *)
let default = {
  name = "<default>";
  perm = Plan9.rw;
  open_ = (fun _ -> ());
  close = (fun _ -> ());
  read_threaded = (fun _ _ _ -> "");
  write_threaded = (fun _ _ _ -> ());
}
(*e: constant [[Device.default]] *)

(*s: exception [[Device.Error]] *)
(* This will be catched up by thread_fileserver to transform the
 * exception in an Rerror 9P response.
 *)
exception Error of string
(*e: exception [[Device.Error]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Device.honor_offset_and_count]] *)
let honor_offset_and_count offset count data =
  let len = String.length data in
  match () with
  | _ when offset > len -> ""
  | _ when offset + count > len ->
    String.sub data offset (len - offset)
  | _ -> data
(*e: function [[Device.honor_offset_and_count]] *)
(*s: function [[Device.honor_count]] *)
let honor_count count data =
  let len = String.length data in
  if len <= count
  then data
  else String.sub data 0 count
(*e: function [[Device.honor_count]] *)
(*e: Device.ml *)
