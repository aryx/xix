open Common

module Unix1 = Unix
module Unix2  = ThreadUnix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* todo? copy https://github.com/mirage/ocaml-9p? but seems heavy
 * on the use of external libraries and modules ...
 * 
 * The format of a 9p message is 
 *  - 32bits int containing the size of the following message
 *  - 8bits type of message
 *  - 16bits tag
 *  - variable bytes depending on the type of the message
 * 
 * later: at some point can replace all the parsing by just using
 * Marshal instead of specialized serialization format
 * (but then can not interact with 9-in-C)
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* todo: use Int32.t *)
type int32 = int
(* todo: use Int64.t *)
type int64 = int

let max_welem = 16

module Request = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach

    | Open
    | Read
    | Write
    | Clunk

    | Walk 
    | Create
    | Remove
    | Stat
    | Wstat

    (* Illegal: Error *)
    | Flush

    | Auth
end

module Response = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach

    | Open
    | Read
    | Write
    | Clunk

    | Walk 
    | Create
    | Remove
    | Stat
    | Wstat

    | Error of string
    | Flush

    | Auth
end

module Q = Request
module R = Response

type message_type =
  | Request of Request.t
  | Response of Response.t

(* old: was called Fcall in libcore-C *)
type message = {

  fid: int32;
  tag: int;

  msg: message_type;
  
}

let bit8sz = 1
let bit16sz = 2
let bit32sz = 4
let bit64sz = 8

(* different from Response.Error *)
exception Error of string

(*****************************************************************************)
(* Dumper *)
(*****************************************************************************)
(* todo: use ocamltarzan at some point and ocaml.ml *)
let str_of_msg msg = 
  (match msg.msg with
  | Request r -> "Q:" ^ 
    (match r with
    | Q.Version (msize, version) -> spf "Version: %d %s" msize version
    | _ -> raise Todo
    )
  | Response r -> "R:" ^
    (match r with
    | R.Version (msize, version) -> spf "Version: %d %s" msize version
    | _ -> raise Todo
    )
  ) ^ spf "(tag = %d, fid = %d)\n" msg.tag msg.fid
    

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* less: factorize with draw_marshal.ml? and formats/executables/a_out.ml ?
 * put in commons/? 
 *)
let i c = Char.code c

(* todo: overflow check! or use Int32 *)
let gbit8 buf off = 
  i buf.[0+off]
let gbit16 buf off =
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8)
let gbit32 buf off =
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8) lor
  (i buf.[2+off] lsl 16) lor (i buf.[3+off] lsl 24)

let gstring buf off =
  let n = gbit16 buf off in
  let off = off + bit16sz in
  String.sub buf off n

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* less: opti: go though a string buffer and size? and convS2M? 
 *  so can reuse the same buffer again and again instead of malloc each time?
 *)
let read_9P_msg fd =
  (* read count *)
  let buf1 = String.make bit32sz ' ' in
  let n = Unix2.read fd buf1 0 bit32sz in
  if n <> bit32sz
  then raise (Error "could not read the message size");
  
  let len = gbit32 buf1 0 in
  if len <= bit32sz
  then raise (Error "bad length in 9P2000 message header");
  let len = len - bit32sz in

  let buf2 = String.make len ' ' in
  let n = Unix2.read fd buf2 0 len in
  if n <> len
  then raise  (Error (spf "could not read enough bytes %d < %d" n len));

  if len < bit8sz + bit16sz
  then raise (Error "no space for message type and tag");
  let buf = buf2 in
  
  let offset = 0 in
  let type_ = gbit8 buf offset in
  let offset = offset + bit8sz in
  let tag   = gbit16 buf 1 in
  let offset = offset + bit16sz in
  let res = { fid = -1; tag = tag; msg = Response (R.Error "TODO") } in

  try (
    match type_ with
    (* Version *)
    | 100 ->
      let msize = gbit32 buf offset in
      let version = gstring buf (offset + 4) in
      { res with msg = Request (Q.Version (msize, version)) }
    | 101 ->
      let msize = gbit32 buf offset in
      let version = gstring buf (offset + 4) in
      { res with msg = Response (R.Version (msize, version)) }
    (* Auth *)
    | 102 -> raise Todo
    (* Attach *)
    | 104 -> raise Todo
    (* Error *)
    | 106 -> raise Todo
    (* Flush *)
    | 108 -> raise Todo
    (* Walk *)
    | 110 -> raise Todo
    (* Open *)
    | 112 -> raise Todo
    (* Create *)
    | 114 -> raise Todo
    (* Read *)
    | 116 -> raise Todo
    (* Write *)
    | 118 -> raise Todo
    (* Clunk *)
    | 120 -> raise Todo
    (* Remove *)
    | 122 -> raise Todo
    (* Stat *)
    | 124 -> raise Todo
    (* Wstat *)
    | 126 -> raise Todo

    | n -> raise (Error (spf "unrecognized message type: %d" n))
  ) 
  with (Invalid_argument s) ->
    raise (Error (spf "access out of range: %s" s))
