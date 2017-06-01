open Common

module Unix1 = Unix
module Unix2  = ThreadUnix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A port of the 9P protocol in OCaml.
 * 
 * The format of a 9P message is 
 *  - 32bits int containing the size of what follows
 *  - 8bits type of message (Txxx or Rxxx)
 *  - 16bits tag
 *  - variable bytes depending on the type of the message
 * 
 * later: at some point can replace all the parsing by just using
 * Marshal instead of specialized serialization format
 * (but then can not interact with 9-in-C)
 * 
 * todo? copy https://github.com/mirage/ocaml-9p? but seems heavy
 * on the use of external libraries and modules ...
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* todo: constructor to sanity check *)
type int16 = int
(* todo: use Int32.t *)
type int32 = int
(* todo: use Int64.t *)
type int64 = int

type fid = int32
type tag = int16
type qid = Plan9.qid
type perm = int32

type open_mode = int

let max_welem = 16

module Request = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of fid (* auth fid *) * string (* uname *) * string (* aname *)

    | Open  of open_mode
    | Read  of int64 (* offset *) * int32 (* count *)
    | Write  of int64 (* offset *) * string (* data *)
    | Clunk of unit

    | Walk   of fid (* newfid *) * string array (* < max_welem *)
    | Create  of string * open_mode * perm
    | Remove  of unit
    | Stat  of unit
    | Wstat  of string (* data, todo: Direntry list *)

    (* Illegal: Error *)
    | Flush  of tag (* oldtag *)

    | Auth  of fid (* auth fid *) * string (* uname *) * string (* aname *)
end 

module Response = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach  of qid

    | Open  of qid * int (* iounit *)
    | Read  of string (* data *)
    | Write  of int (* count *)
    | Clunk  of unit

    | Walk  of qid array (* < max_welem *)
    | Create of qid * int (* iounit *)
    | Remove of unit
    | Stat of string (* data, todo: Direntry list *)
    | Wstat of unit

    | Error of string
    | Flush of unit

    | Auth of qid (* auth qid *)
end

(* old: the requests are prefixed with a T *)
module T = Request
module R = Response

type message_type =
  | T of T.t
  | R of R.t

(* old: was called Fcall in libcore-C *)
type message = {

  fid: fid;
  tag: tag;

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
let str_of_qid x =
  raise Todo
let str_of_mode x =
  raise Todo
let str_of_perm x =
  spf "%d" x


(* todo: use ocamltarzan at some point and ocaml.ml *)
let str_of_msg msg = 
  (match msg.msg with
  | T r -> "Q:" ^ 
    (match r with
    | T.Version (msize, version) -> 
      spf "Version: %d %s" msize version
    | T.Attach (afid, uname, aname) -> 
      spf "Attach: auth_fid = %d uname = %s aname = %s" afid uname aname
    | T.Auth (afid, uname, aname) -> 
      spf "Auth: auth_fid = %d uname = %s aname = %s" afid uname aname
    | T.Flush oldtag -> 
      spf "Flush: old_tag = %d" oldtag
    | T.Walk (newfid, arr) -> 
      spf "Walk: new_fid = %d, [|%s|]" newfid 
        (arr |> Array.to_list |> String.concat ", ")
    | T.Open mode -> 
      spf "Walk: mode = %s" (str_of_mode mode)
    | T.Create (name, mode, perm) -> 
      spf "Create: %s, mode = %s, perm = %s" name (str_of_mode mode) 
        (str_of_perm perm)
    | T.Read (offset, count) ->
      spf "Read: offset = %d, count = %d" offset count
    | T.Write (offset, data) ->
      spf "Write: offset = %d, data = %s" offset
        (if String.length data > 10 then String.sub data 0 10 else data)
    | T.Clunk () ->
      spf "Clunk: "
    | T.Remove () ->
      spf "Remove: "
    | T.Stat () ->
      spf "Stat: "
    | T.Wstat str -> 
      spf  "Wstat: %s"
        (if String.length str > 10 then String.sub str 0 10 else str)
    )
  | R r -> "R:" ^
    (match r with
    | R.Version (msize, version) -> 
      spf "Version: %d %s" msize version
    | R.Attach qid -> 
      spf "Attach: %s" (str_of_qid qid)
    | R.Auth qid -> 
      spf "Auth: %s" (str_of_qid qid)
    | R.Error str -> 
      spf "Error: %s" str
    | R.Flush () -> 
      spf "Flush:"
    | R.Open (qid, iounit) -> 
      spf "Open: qid = %s, iounit = %d" (str_of_qid qid) iounit
    | R.Create (qid, iounit) -> 
      spf "Create: qid = %s, iounit = %d" (str_of_qid qid) iounit
    | R.Read str ->
      spf "Read: %s" 
        (if String.length str > 10 then String.sub str 0 10 else str)
    | R.Write count -> 
      spf  "Write: %d" count
    | R.Clunk () -> 
      spf  "Clunk:"
    | R.Walk (arr) -> 
      spf "Walk: [|%s|]"
        (arr |> Array.to_list |> List.map str_of_qid |> String.concat ", ")
    | R.Remove () -> 
      spf  "Remove:"
    | R.Stat str -> 
      spf  "Stat: %s"
        (if String.length str > 10 then String.sub str 0 10 else str)
    | R.Wstat () -> 
      spf  "Wstat:"
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
(* Entry points *)
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
  let res = { fid = -1; tag = tag; msg = R (R.Error "TODO") } in
  try (
    match type_ with
    (* Version *)
    | 100 ->
      let msize = gbit32 buf offset in
      let version = gstring buf (offset + 4) in
      { res with msg = T (T.Version (msize, version)) }
    | 101 ->
      let msize = gbit32 buf offset in
      let version = gstring buf (offset + 4) in
      { res with msg = R (R.Version (msize, version)) }
    (* Auth *)
    | 102 -> raise (Error (spf "R: %d" type_))
    (* Attach *)
    | 104 -> 
      let fid = gbit32 buf offset in
      let afid = gbit32 buf (offset + 4) in
      let uname = gstring buf (offset + 8) in
      let offset = (offset + 8 + String.length uname + bit16sz) in
      let aname = gstring buf offset in
      { res with fid = fid; msg = T (T.Attach (afid, uname, aname)) }
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
  | T  (T.Version _) -> 100
  | R (R.Version _) -> 101
  | T  (T.Auth _) -> 102
  | R (R.Auth _) -> 103
  | T  (T.Attach _) -> 104
  | R (R.Attach _) -> 105
(*  | T  (T.Error _) -> 106 *)
  | R (R.Error _) -> 107
  | T  (T.Flush _) -> 108
  | R (R.Flush _) -> 109
  | T  (T.Walk _) -> 110
  | R (R.Walk _) -> 111
  | T  (T.Open _) -> 112
  | R (R.Open _) -> 113
  | T  (T.Create _) -> 114
  | R (R.Create _) -> 115
  | T  (T.Read _) -> 116
  | R (R.Read _) -> 117
  | T  (T.Write _) -> 118
  | R (R.Write _) -> 119
  | T  (T.Clunk _) -> 120
  | R (R.Clunk _) -> 121
  | T  (T.Remove _) -> 122
  | R (R.Remove _) -> 123
  | T  (T.Stat _) -> 124
  | R (R.Stat _) -> 125
  | T  (T.Wstat _) -> 126
  | R (R.Wstat _) -> 126

(* less: opti: use a string buffer instead of all those concatenations *)
let write_9P_msg msg fd =
  let type_ = type_of_msg msg.msg in
  let str = 
    pbit8 type_ ^ 
    pbit16 msg.tag ^ 
    (match msg.msg with
    | T x ->
      (match x with
      | T.Version (msize, version) -> raise (Error (spf "%d" type_))
      | _ -> raise (Error (spf "W: %d" type_))
      )
    | R x ->
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
