open Common

module N = Plan9

module Unix1 = Unix
module Unix2  = ThreadUnix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A port of the 9P protocol in OCaml.
 * 
 * The format of a 9P message is 
 *  - 32 bits: size of what follows
 *  - 8 bits: type of the message (Txxx or Rxxx)
 *  - 16 bits: tag of the message
 *  - variable bytes: depends on the type of the message
 * 
 * later: at some point we can replace all the parsing by just using
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

(* less: reuse Unix.open_flag? *)
type open_mode = int

let max_welem = 16

module Request = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of fid * fid (* auth_fid *) * string (* user *) * string (*aname*)

    | Flush of tag (* old_tag *)
    | Auth of fid (* auth_fid *) * string (* user *) * string (* aname *)
    (* Note that Error is not here (it can just be a Response, not a Request) *)

    | Open of fid * open_mode
    | Read of fid * int64 (* offset *) * int32 (* count *)
    | Write of fid * int64 (* offset *) * string (* data *)
    | Clunk of fid

    | Walk  of fid * fid (* newfid *) * string list (* < max_welem *)
    | Create of fid * string * open_mode * perm
    | Remove of fid
    | Stat of fid
    | Wstat of fid * string (* todo: Direntry list *)

end 

module Response = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of qid

    | Error of string
    | Flush of unit

    | Auth of qid (* auth_qid *)

    | Open of qid * int (* iounit *)
    | Read of string (* data *)
    | Write of int (* count *)
    | Clunk of unit

    | Walk of qid list (* < max_welem *)
    | Create of qid * int (* iounit *)
    | Remove of unit
    | Stat of string (* data, todo: Direntry list *)
    | Wstat of unit

end

(* old: the requests are prefixed with a T in libcore-C *)
module T = Request
module R = Response

type message_type =
  | T of T.t
  | R of R.t

(* old: was called Fcall in libcore-C *)
type message = {
  tag: tag;
  typ: message_type;
}

(* less: could remove, faster to do '+ 4' than '+ bit32sz' *)
let bit8sz = 1
let bit16sz = 2
let bit32sz = 4
let bit64sz = 8

(* different from Response.Error *)
exception Error of string

let debug = ref false

(*****************************************************************************)
(* Dumper *)
(*****************************************************************************)
(* less: 'd', 'a', 'l', 'A' instead of integer for qid type *)
let str_of_qid qid =
  spf "path = %d, vers = %d, type = %d" qid.N.path qid.N.vers
    (N.int_of_qid_type qid.N.typ)

let str_of_mode x =
  spf "%d" x
let str_of_perm x =
  spf "%d" x


(* todo: use ocamltarzan at some point and ocaml.ml *)
let str_of_msg msg = 
  (match msg.typ with
  | T r -> "Q:" ^ 
    (match r with
    | T.Version (msize, version) -> 
      spf "Version: msize = %d version = %s" msize version
    | T.Attach (fid, afid, uname, aname) -> 
      spf "Attach: fid = %d auth_fid = %d uname = %s aname = %s" 
        fid afid uname aname
    | T.Auth (afid, uname, aname) -> 
      spf "Auth: auth_fid = %d uname = %s aname = %s" afid uname aname
    | T.Flush oldtag -> 
      spf "Flush: old_tag = %d" oldtag
    | T.Walk (fid, newfid, xs) -> 
      spf "Walk: fid = %d new_fid = %d [%s]" fid newfid 
        (xs |> String.concat ", ")
    | T.Open (fid, mode) -> 
      spf "Walk: fid = %d mode = %s" fid (str_of_mode mode)
    | T.Create (fid, name, mode, perm) -> 
      spf "Create: fid = %d, name = %s, mode = %s, perm = %s" 
        fid name (str_of_mode mode) 
        (str_of_perm perm)
    | T.Read (fid, offset, count) ->
      spf "Read: fid = %d offset = %d, count = %d" fid offset count
    | T.Write (fid, offset, data) ->
      spf "Write: fid = %d offset = %d, data = %s" fid offset
        (if String.length data > 10 then String.sub data 0 10 else data)
    | T.Clunk fid ->
      spf "Clunk: %d" fid
    | T.Remove fid ->
      spf "Remove: %d" fid
    | T.Stat fid ->
      spf "Stat: %d" fid
    | T.Wstat (fid, str) -> 
      spf  "Wstat: fid = %d, %s" fid
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
    | R.Walk (xs) -> 
      spf "Walk: [%s]"
        (xs |> List.map str_of_qid |> String.concat ", ")
    | R.Remove () -> 
      spf  "Remove:"
    | R.Stat str -> 
      spf  "Stat: %s"
        (if String.length str > 10 then String.sub str 0 10 else str)
    | R.Wstat () -> 
      spf  "Wstat:"
    )
  ) ^ spf " (tag = %d)" msg.tag
    

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

(* todo: use Int64 *)
let gbit64 buf off =
  let n = 
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8) lor
  (i buf.[2+off] lsl 16) lor (i buf.[3+off] lsl 24)
  in
  (match buf.[4], buf.[5], buf.[6], buf.[7] with
  | '\000', '\000', '\000', '\000' -> 
    n
  | _ -> failwith "TODO: gbit64 overflow"
  )

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

(* todo: sanity check range *)
let pbit64 x =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = Char.chr ((x asr 24) land 0xFF) in
  let str = String.make 8 ' ' in
  str.[0] <- x1;
  str.[1] <- x2;
  str.[2] <- x3;
  str.[3] <- x4;
  str.[4] <- Char.chr 0;
  str.[5] <- Char.chr 0;
  str.[6] <- Char.chr 0;
  str.[7] <- Char.chr 0;
  str

let pstring s =
  let len = String.length s in
  pbit16 len ^ s

let pqid qid = 
  pbit8 (Plan9.int_of_qid_type qid.N.typ) ^
  pbit32 qid.N.vers ^
  pbit64 qid.N.path

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
  let tag   = gbit16 buf offset in
  let offset = offset + bit16sz in
  let res = { tag = tag; typ = R (R.Error "TODO") } in
  try (
    let msg, final_offset = 
    match type_ with
    (* Version *)
    | 100 ->
      let msize = gbit32 buf offset in
      let version = gstring buf (offset + 4) in
      { res with typ = T (T.Version (msize, version)) },
      offset + 4 + String.length version + bit16sz
    (* Auth *)
    | 102 -> 
      let afid = gbit32 buf offset in
      let uname = gstring buf (offset + 4) in
      let offset = offset + 4 + String.length uname + bit16sz in
      let aname = gstring buf offset in
      { res with typ = T (T.Auth (afid, uname, aname)) },
      offset + String.length aname + bit16sz
    (* Attach *)
    | 104 -> 
      let fid = gbit32 buf offset in
      let afid = gbit32 buf (offset + 4) in
      let uname = gstring buf (offset + 8) in
      let offset = offset + 8 + String.length uname + bit16sz in
      let aname = gstring buf offset in
      { res with typ = T (T.Attach (fid, afid, uname, aname)) },
      offset + String.length aname + bit16sz
    (* Error *)
    | 106 -> raise (Impossible "There is no T.Error")
    (* Flush *)
    | 108 -> 
      let oldtag = gbit16 buf offset in
      { res with typ = T (T.Flush (oldtag)) },
      offset + 2
    (* Walk *)
    | 110 -> 
      let fid = gbit32 buf offset in
      let newfid = gbit32 buf (offset + 4) in
      let nwname = gbit16 buf (offset + 8) in
      if nwname > max_welem
      then failwith "too many names";
      let xs = ref [] in
      let offset = ref (offset + 10) in
      for i = 0 to nwname - 1 do
        let str = gstring buf !offset in
        let len = String.length str + bit16sz in
        offset := !offset + len;
        xs := str::!xs;
      done;
      { res with typ = T (T.Walk (fid, newfid, List.rev !xs)) },
      !offset
    (* Open *)
    | 112 -> 
      let fid = gbit32 buf offset in
      let mode = gbit8 buf (offset + 4) in
      { res with typ = T (T.Open (fid, mode)) },
      offset + 5
    (* create *)
    | 114 -> 
      let fid = gbit32 buf offset in
      let name = gstring buf (offset + 4) in
      let offset = offset + 4 + String.length name + bit16sz in
      let perm = gbit32 buf offset in
      let mode = gbit8 buf (offset + 4) in
      (* I reverse the order between mode and perm in Create, to match
       * more closely Unix.open *)
      { res with typ = T (T.Create (fid, name, mode, perm)) },
      offset + 5
    (* Read *)
    | 116 -> 
      let fid = gbit32 buf offset in
      let offset = gbit64 buf (offset + 4) in
      let count = gbit32 buf (offset + 12) in
      { res with typ = T (T.Read (fid, offset, count)) },
      offset + 16
    (* Write *)
    | 118 -> 
      let fid = gbit32 buf offset in
      let offset = gbit64 buf (offset + 4) in
      let count = gbit32 buf (offset + 12) in
      let data = String.sub buf (offset + 16) count in
      { res with typ = T (T.Write (fid, offset, data)) },
      offset + 16 + count
    (* Clunk *)
    | 120 -> 
      let fid = gbit32 buf offset in
      { res with typ = T (T.Clunk (fid)) },
      offset + 4
    (* Remove *)
    | 122 -> 
      let fid = gbit32 buf offset in
      { res with typ = T (T.Remove (fid)) },
      offset + 4
    (* Stat *)
    | 124 -> 
      let fid = gbit32 buf offset in
      { res with typ = T (T.Stat (fid)) },
      offset + 4
    (* Wstat *)
    | 126 -> 
      let fid = gbit32 buf offset in
      let nstat = gbit16 buf (offset + 4) in
      let data = String.sub buf (offset + 6) nstat in
      { res with typ = T (T.Wstat (fid, data)) },
      offset + 6 + nstat

    | n -> raise (Error (spf "unrecognized message type: %d" n))
    in
    if final_offset < len
    then raise (Error "did not read enough bytes");
    if final_offset > len
    then raise (Error "read to many bytes");
    msg
  )
  with (Invalid_argument s) ->
    raise (Error (spf "access out of range: %s" s))


let code_of_msg msg = 
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
  let code = code_of_msg msg.typ in
  let str = 
    pbit8 code ^ 
    pbit16 msg.tag ^ 
    (match msg.typ with
    | R x ->
      (match x with 

      | R.Version (msize, version) -> pbit32 msize ^ pstring version
      | R.Attach qid -> pqid qid
      | R.Error str -> pstring str
      | R.Flush () -> ""
      | R.Auth auth_qid -> pqid auth_qid
      | R.Open (qid, iounit) -> pqid qid ^ pbit32 iounit
      | R.Create (qid, iounit) -> pqid qid ^ pbit32 iounit
      | R.Read data -> 
        let len = String.length data in
        pbit32 len ^ data
      | R.Write count -> pbit32 count
      | R.Clunk () -> ""
      | R.Walk xs -> 
        let len = List.length xs in
        assert (len < max_welem);
        pbit16 len ^ (xs |> List.map pqid |> String.concat "")
      | R.Remove () -> ""
      | R.Stat data -> 
        let nstat = String.length data in
        pbit16 nstat ^ data
      | R.Wstat () -> ""
      )
    | T x ->
      (match x with
      | _ -> raise (Error (spf "W: %d" code))
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
