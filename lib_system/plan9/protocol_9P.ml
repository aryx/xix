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
    | Attach of int32 (* auth fid *) * string (* uname *) * string (* aname *)

    | Open  of unit
    | Read  of unit
    | Write  of unit
    | Clunk  of unit

    | Walk   of unit
    | Create  of unit
    | Remove  of unit
    | Stat  of unit
    | Wstat  of unit

    (* Illegal: Error *)
    | Flush  of unit

    | Auth  of unit
end 

module Response = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach  of unit

    | Open  of unit
    | Read  of unit
    | Write  of unit
    | Clunk  of unit

    | Walk  of unit
    | Create of unit
    | Remove of unit
    | Stat of unit
    | Wstat of unit

    | Error of string
    | Flush of unit

    | Auth of unit
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

(* less: could remove, faster to do '+ 4' than '+ bit32sz' *)
let bit8sz = 1
let bit16sz = 2
let bit32sz = 4
let bit64sz = 8

(* different from Response.Error *)
exception Error of string

let debug = ref true

(*****************************************************************************)
(* Dumper *)
(*****************************************************************************)
(* todo: use ocamltarzan at some point and ocaml.ml *)
let str_of_msg msg = 
  (match msg.msg with
  | Request r -> "Q:" ^ 
    (match r with
    | Q.Version (msize, version) -> 
      spf "Version: %d %s" msize version
    | Q.Attach (afid, uname, aname) -> 
      spf "Attach: auth_fid = %d uname = %s aname = %s" afid uname aname
    | _ -> raise Todo
    )
  | Response r -> "R:" ^
    (match r with
    | R.Version (msize, version) -> spf "Version: %d %s" msize version
    | _ -> raise Todo
    )
  ) ^ spf " (tag = %d, fid = %d)\n" msg.tag msg.fid
    

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
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

(* less: factorize with draw_marshal.ml? and formats/executables/a_out.ml ?
 * put in commons/? 
 *)
let pbit8 x = 
  String.make 1 (Char.chr x)

let pbit16 x =
  (* less: sanity check range? *)
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let str = String.make 2 ' ' in
  str.[0] <- x1;
  str.[1] <- x2;
  str

let pbit32 x =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = Char.chr ((x asr 24) land 0xFF) in
  let str = String.make 4 ' ' in
  str.[0] <- x1;
  str.[1] <- x2;
  str.[2] <- x3;
  str.[3] <- x4;
  str

let pstring s =
  let len = String.length s in
  pbit16 len ^ s

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
  if !debug then pr (spf "read in total %d" (n+bit32sz));

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
    | 102 -> raise (Error (spf "R: %d" type_))
    (* Attach *)
    | 104 -> 
      let fid = gbit32 buf offset in
      let afid = gbit32 buf (offset + 4) in
      let uname = gstring buf (offset + 8) in
      let offset = (offset + 8 + String.length uname + bit16sz) in
      let aname = gstring buf offset in
      { res with fid = fid; msg = Request (Q.Attach (afid, uname, aname)) }
    (* Error *)
    | 106 -> raise (Error (spf "R: %d" type_))
    (* Flush *)
    | 108 -> raise (Error (spf "R: %d" type_))
    (* Walk *)
    | 110 -> raise (Error (spf "R: %d" type_))
    (* Open *)
    | 112 -> raise (Error (spf "R: %d" type_))
    (* Create *)
    | 114 -> raise (Error (spf "R: %d" type_))
    (* Read *)
    | 116 -> raise (Error (spf "R: %d" type_))
    (* Write *)
    | 118 -> raise (Error (spf "R: %d" type_))
    (* Clunk *)
    | 120 -> raise (Error (spf "R: %d" type_))
    (* Remove *)
    | 122 -> raise (Error (spf "R: %d" type_))
    (* Stat *)
    | 124 -> raise (Error (spf "R: %d" type_))
    (* Wstat *)
    | 126 -> raise (Error (spf "R: %d" type_))

    | n -> raise (Error (spf "unrecognized message type: %d" n))
  )
  with (Invalid_argument s) ->
    raise (Error (spf "access out of range: %s" s))


let type_of_msg msg = 
  match msg with
  | Request  (Q.Version _) -> 100
  | Response (R.Version _) -> 101
  | Request  (Q.Auth _) -> 102
  | Response (R.Auth _) -> 103
  | Request  (Q.Attach _) -> 104
  | Response (R.Attach _) -> 105
(*  | Request  (Q.Error _) -> 106 *)
  | Response (R.Error _) -> 107
  | Request  (Q.Flush _) -> 108
  | Response (R.Flush _) -> 109
  | Request  (Q.Walk _) -> 110
  | Response (R.Walk _) -> 111
  | Request  (Q.Open _) -> 112
  | Response (R.Open _) -> 113
  | Request  (Q.Create _) -> 114
  | Response (R.Create _) -> 115
  | Request  (Q.Read _) -> 116
  | Response (R.Read _) -> 117
  | Request  (Q.Write _) -> 118
  | Response (R.Write _) -> 119
  | Request  (Q.Clunk _) -> 120
  | Response (R.Clunk _) -> 121
  | Request  (Q.Remove _) -> 122
  | Response (R.Remove _) -> 123
  | Request  (Q.Stat _) -> 124
  | Response (R.Stat _) -> 125
  | Request  (Q.Wstat _) -> 126
  | Response (R.Wstat _) -> 126

(* less: opti: use a string buffer instead of all those concatenations *)
let write_9P_msg msg fd =
  let type_ = type_of_msg msg.msg in
  let str = 
    pbit8 type_ ^ 
    pbit16 msg.tag ^ 
    (match msg.msg with
    | Request x ->
      (match x with
      | Q.Version (msize, version) -> raise (Error (spf "%d" type_))
      | _ -> raise (Error (spf "W: %d" type_))
      )
    | Response x ->
      (match x with 
      | R.Version (msize, version) -> 
        pbit32 msize ^ pstring version
      | _ -> raise (Error (spf "W: %d" type_))
      )
    )
  in
  let len = String.length str + bit32sz in
  let str = pbit32 len ^ str in
  if !debug then pr (spf "write in total %d" len);
  let n = Unix2.write fd str 0 len in
  if n <> len
  then failwith "write error in 9P response";
  ()
