open Common

type t = {
  name: string;
  perm: Plan9.perm_property;

  open_: Window.t -> unit;
  close: Window.t -> unit;
  
  (* we need to thread 'read' because this operation may spend some
   * time waiting while receiving and sending information on channels.
   * 
   * Note that 'read' takes an offset (and count) because each device 
   * can honor or not the offset requirements. For instance 
   * /dev/mouse does not honor offset but /dev/winname does.
   *)
  read_threaded: int64 -> int -> Window.t -> bytes;
  write_threaded: int64 -> bytes -> Window.t -> unit;
}

let default = {
  name = "<default>";
  perm = Plan9.rw;
  open_ = (fun _ -> ());
  close = (fun _ -> ());
  read_threaded = (fun _ _ _ -> "");
  write_threaded = (fun _ _ _ -> ());
}

(* This will be catched up by thread_fileserver to transform the
 * exception in an Rerror 9P response.
 *)
exception Error of string

(* helpers *)
let honor_offset_and_count offset count data =
  let len = String.length data in
  match () with
  | _ when offset > len -> ""
  | _ when offset + count > len ->
    String.sub data offset (len - offset)
  | _ -> data

let honor_count count data =
  let len = String.length data in
  if len <= count
  then data
  else String.sub data 0 count
