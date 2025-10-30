open Common

module N = Plan9

module Unix1 = Unix
module Unix2  = (*Thread*)Unix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A port of the 9P protocol in OCaml.
 * 
 * The format of a 9P message is 
 *  - 32 bits: size of what follows
 *  - 8 bits: type of the message (Txxx or Rxxx)
 *  - 16 bits: tag of the message
 *  - the rest depends on the type of the message
 * 
 * See 0intro(5) in the man page of Plan9 for more information on 9P.
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

type fid = int32
type tag = int16
type qid = Plan9.qid

let max_welem = 16

module Request = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of fid * fid option (* auth_fid *) * 
                string (* user *) * string (*aname*)

    (* note that it's Open of fid not string, so you need to have 'walked'
     * before that fid to the path you want.
     *)
    | Open of fid * Plan9.open_flags
    | Read of fid * int64 (* offset *) * int32 (* count *)
    | Write of fid * int64 (* offset *) * bytes (* data *)
    | Clunk of fid

    | Walk  of fid * fid option (* newfid when clone *) * 
               string list (* < max_welem *)
    | Create of fid * string * Plan9.open_flags * Plan9.perm_int
    | Remove of fid
    | Stat of fid
    | Wstat of fid * Plan9.dir_entry

    | Flush of tag (* old_tag *)
    | Auth of fid (* auth_fid *) * string (* user *) * string (* aname *)
    (* Note that Error is not here (it can just be a Response, not a Request) *)
end 

module Response = struct
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of qid

    | Error of string

    | Open of qid * int (* iounit *)
    | Read of bytes (* data *)
    | Write of int (* count *)
    | Clunk

    | Walk of qid list (* < max_welem *)
    | Create of qid * int (* iounit *)
    | Remove
    | Stat of Plan9.dir_entry
    | Wstat

    | Flush
    | Auth of qid (* auth_qid *)

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

let io_header_size = 24

(* different from Response.Error *)
exception Error of string

(*****************************************************************************)
(* Dumper *)
(*****************************************************************************)
(* less: 'd', 'a', 'l', 'A' instead of integer for qid type *)
let str_of_qid qid =
  spf "path = %d, vers = %d, type = %d" qid.N.path qid.N.vers
    (N.int_of_qid_type qid.N.typ)

let str_of_mode x =
  spf "%c%c%c" 
    (if x.N.r then 'r' else '_')
    (if x.N.w then 'w' else '_')
    (if x.N.x then 'x' else '_')
let str_of_perm_int x =
  spf "%d" x


(* alt: use deriving *)
let str_of_msg msg = 
  (match msg.typ with
  | T r -> "Q:" ^ 
    (match r with
    | T.Version (msize, version) -> 
      spf "Version: msize = %d version = %s" msize version
    | T.Attach (fid, afid, uname, aname) -> 
      spf "Attach: fid = %d auth_fid = %d uname = %s aname = %s" 
        fid (match afid with None -> -1 | Some x -> x) uname aname
    | T.Auth (afid, uname, aname) -> 
      spf "Auth: auth_fid = %d uname = %s aname = %s" afid uname aname
    | T.Flush oldtag -> 
      spf "Flush: old_tag = %d" oldtag
    | T.Walk (fid, newfid_opt, xs) -> 
      spf "Walk: fid = %d new_fid = %d [%s]" fid 
        (match newfid_opt with None -> fid | Some x -> x)
        (xs |> String.concat ", ")
    | T.Open (fid, mode) -> 
      spf "Open: fid = %d mode = %s" fid (str_of_mode mode)
    | T.Create (fid, name, mode, perm) -> 
      spf "Create: fid = %d, name = %s, mode = %s, perm = %s" 
        fid name (str_of_mode mode) 
        (str_of_perm_int perm)
    | T.Read (fid, offset, count) ->
      spf "Read: fid = %d offset = %d, count = %d" fid offset count
    | T.Write (fid, offset, data) ->
      spf "Write: fid = %d offset = %d, data = %s" fid offset
        (if Bytes.length data > 10 then Bytes.sub_string data 0 10 else Bytes.to_string data)
    | T.Clunk fid ->
      spf "Clunk: %d" fid
    | T.Remove fid ->
      spf "Remove: %d" fid
    | T.Stat fid ->
      spf "Stat: %d" fid
    | T.Wstat (fid, entry) -> 
      spf  "Wstat: fid = %d, name = %s" fid entry.N.name
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
    | R.Flush -> 
      spf "Flush:"
    | R.Open (qid, iounit) -> 
      spf "Open: qid = %s, iounit = %d" (str_of_qid qid) iounit
    | R.Create (qid, iounit) -> 
      spf "Create: qid = %s, iounit = %d" (str_of_qid qid) iounit
    | R.Read str ->
      spf "Read: %s" 
        (if Bytes.length str > 10 then Bytes.sub_string str 0 10 else Bytes.to_string str)
    | R.Write count -> 
      spf  "Write: %d" count
    | R.Clunk -> 
      spf  "Clunk:"
    | R.Walk (xs) -> 
      spf "Walk: [%s]"
        (xs |> List.map str_of_qid |> String.concat ", ")
    | R.Remove -> 
      spf  "Remove:"
    | R.Stat entry -> 
      spf  "Stat: name = %s" entry.N.name
    | R.Wstat -> 
      spf  "Wstat:"
    )
  ) ^ spf " (tag = %d)" msg.tag
    

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let i c = Char.code c

(* 9P uses little-endian order (least significant byte first) *)

let gbit8 buf off = 
  i buf.[0+off]
let gbit16 buf off =
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8)
(* todo: overflow check! or use Int32 *)
let gbit32 buf off =
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8) lor
  (i buf.[2+off] lsl 16) lor (i buf.[3+off] lsl 24)
(* todo: use Int64 *)
let gbit64 buf off =
  let n1 = 
  (i buf.[0+off])        lor (i buf.[1+off] lsl 8) lor
  (i buf.[2+off] lsl 16) lor (i buf.[3+off] lsl 24)
  in
  let n2 = 
  (i buf.[4+off])        lor (i buf.[5+off] lsl 8) lor
  (i buf.[6+off] lsl 16) lor (i buf.[7+off] lsl 24)
  in
  (* bugfix: n2, n1,   not n1, n2!  assume later do 'let (high,low) = offset'*)
  (* old: n2, n1 *)
  if n2 > 0
  then failwith "gbit64: TODO support for high value with Int64";
  (* todo: what about negative values where n2 is just 0b1111111111... ? *)
  n1

let gstring buf off =
  let n = gbit16 buf off in
  let off = off + bit16sz in
  String.sub buf off n

let gdir_entry _buf =
  raise Todo





(* less: factorize with draw_marshal.ml? and formats/executables/a_out.ml ?
 * put in commons/? 
 *)
let pbit8 x = 
  String.make 1 (Char.chr x)

let pbit16 x =
  (* less: sanity check range? *)
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let str = Bytes.make 2 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.to_string str

let pbit32 x =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = Char.chr ((x asr 24) land 0xFF) in
  let str = Bytes.make 4 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.set str 2 x3;
  Bytes.set str 3 x4;
  Bytes.to_string str

let pbit32_special (x, dm) =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = 
    match dm with
    | N.DMDir -> Char.chr  (((x asr 24) land 0xFF) lor 128)
    | N.DMFile -> Char.chr ((x asr 24) land 0xFF)
  in
  let str = Bytes.make 4 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.set str 2 x3;
  Bytes.set str 3 x4;
  Bytes.to_string str
  

(* todo: sanity check range *)
let pbit64 x =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = Char.chr ((x asr 24) land 0xFF) in
  let str = Bytes.make 8 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.set str 2 x3;
  Bytes.set str 3 x4;
  Bytes.set str 4 (Char.chr 0);
  Bytes.set str 5 (Char.chr 0);
  Bytes.set str 6 (Char.chr 0);
  Bytes.set str 7 (Char.chr 0);
  Bytes.to_string str

let pstring s =
  let len = String.length s in
  pbit16 len ^ s

let pqid qid = 
  pbit8 (Plan9.int_of_qid_type qid.N.typ) ^
  pbit32 qid.N.vers ^
  pbit64 qid.N.path


let qidsz = bit8sz + bit32sz + bit64sz
let stat_fixed_sz = bit16sz + qidsz + 5 * bit16sz + 4 * bit32sz + 1 * bit64sz

let pdir_entry entry =
  let fixed_sz = stat_fixed_sz in
  let variable_sz = 
    String.length entry.N.name +
    String.length entry.N.uid +
    String.length entry.N.gid +
    String.length entry.N.muid
  in
  let total_sz = fixed_sz + variable_sz in
  (* the count does not include the size of the count itself *)
  pbit16 (total_sz - bit16sz) ^
  pbit16 entry.N._typ ^ 
  pbit32 entry.N._dev ^
  pqid entry.N.qid ^
  pbit32_special entry.N.mode ^
  pbit32 entry.N.atime ^
  pbit32 entry.N.mtime ^
  pbit64 entry.N.length ^
  ([entry.N.name; entry.N.uid; entry.N.gid; entry.N.muid] |>
   List.map (fun str -> pbit16 (String.length str) ^ str) |> String.concat "")


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* less: opti: go though a string buffer and size? and convS2M? 
 *  so can reuse the same buffer again and again instead of malloc each time?
 *)
let read_9P_msg fd =
  (* read count *)
  let buf1 = Bytes.make bit32sz ' ' in
  let n = Unix2.read fd buf1 0 bit32sz in
  if n <> bit32sz
  then raise (Error "could not read the message size");
  
  let len = gbit32 (Bytes.to_string buf1) 0 in
  if len <= bit32sz
  then raise (Error "bad length in 9P2000 message header");
  let len = len - bit32sz in

  let buf2 = Bytes.make len ' ' in
  let n = Unix2.read fd buf2 0 len in
  if n <> len
  then raise  (Error (spf "could not read enough bytes %d < %d" n len));
  Logs.debug (fun m -> m "read in total %d" (n+bit32sz));

  if len < bit8sz + bit16sz
  then raise (Error "no space for message type and tag");
  let buf = buf2 in
  
  let offset = 0 in
  let type_ = gbit8 (Bytes.to_string buf) offset in
  let offset = offset + bit8sz in
  let tag   = gbit16 (Bytes.to_string buf) offset in
  let offset = offset + bit16sz in
  let res = { tag = tag; typ = R (R.Error "TODO") } in

  let buf = Bytes.to_string buf in
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
      let afidopt = if afid = -1 then None else Some afid in
      let uname = gstring buf (offset + 8) in
      let offset = offset + 8 + String.length uname + bit16sz in
      let aname = gstring buf offset in
      { res with typ = T (T.Attach (fid, afidopt, uname, aname)) },
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
      for _i = 0 to nwname - 1 do
        let str = gstring buf !offset in
        let len = String.length str + bit16sz in
        offset := !offset + len;
        xs := str::!xs;
      done;
      let newfid_opt = if newfid = fid then None else Some newfid in
      { res with typ = T (T.Walk (fid, newfid_opt, List.rev !xs)) },
      !offset
    (* Open *)
    | 112 -> 
      let fid = gbit32 buf offset in
      let mode = gbit8 buf (offset + 4) in
      let flags = N.open_flags_of_int mode in
      { res with typ = T (T.Open (fid, flags)) },
      offset + 5
    (* create *)
    | 114 -> 
      let fid = gbit32 buf offset in
      let name = gstring buf (offset + 4) in
      let offset = offset + 4 + String.length name + bit16sz in
      let perm = gbit32 buf offset in
      let mode = gbit8 buf (offset + 4) in
      let flags = N.open_flags_of_int mode in
      (* I reverse the order between mode and perm in Create, to match
       * more closely Unix.open *)
      { res with typ = T (T.Create (fid, name, flags, perm)) },
      offset + 5
    (* Read *)
    | 116 -> 
      let fid = gbit32 buf offset in
      (* bugfix: not to confuse with 'offset' used to index buf *)
      let read_offset = gbit64 buf (offset + 4) in
      let count = gbit32 buf (offset + 12) in
      { res with typ = T (T.Read (fid, read_offset, count)) },
      offset + 16
    (* Write *)
    | 118 -> 
      let fid = gbit32 buf offset in
      let write_offset = gbit64 buf (offset + 4) in
      let count = gbit32 buf (offset + 12) in
      let data = String.sub buf (offset + 16) count in
      { res with typ = T (T.Write (fid, write_offset, Bytes.of_string data)) },
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
      let entry = gdir_entry data in
      { res with typ = T (T.Wstat (fid, entry)) },
      offset + 6 + nstat

    | n -> raise (Error (spf "unrecognized message type: %d" n))
    in
    if final_offset < len
    then raise (Error 
      (spf "did not read enough bytes, final = %d < len = %d (op = %d)"
         final_offset len type_));
    if final_offset > len
    then raise (Error 
      (spf "read to many bytes, final = %d > len = %d (op = %d)"
         final_offset len type_));
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
  | R (R.Flush) -> 109
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
  | R (R.Clunk) -> 121
  | T  (T.Remove _) -> 122
  | R (R.Remove) -> 123
  | T  (T.Stat _) -> 124
  | R (R.Stat _) -> 125
  | T  (T.Wstat _) -> 126
  | R (R.Wstat) -> 126

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
      | R.Flush -> ""
      | R.Auth auth_qid -> pqid auth_qid
      | R.Open (qid, iounit) -> pqid qid ^ pbit32 iounit
      | R.Create (qid, iounit) -> pqid qid ^ pbit32 iounit
      | R.Read data -> 
        let len = Bytes.length data in
        pbit32 len ^ (Bytes.to_string data)
      | R.Write count -> pbit32 count
      | R.Clunk -> ""
      | R.Walk xs -> 
        let len = List.length xs in
        assert (len < max_welem);
        pbit16 len ^ (xs |> List.map pqid |> String.concat "")
      | R.Remove -> ""
      | R.Stat entry -> 
        let data = pdir_entry entry in
        let nstat = String.length data in
        pbit16 nstat ^ data
      | R.Wstat -> ""
      )
    | T x ->
      (match x with
      | _ -> raise (Error (spf "W: %d" code))
      )
    )
  in
  let len = String.length str + bit32sz in
  let str = pbit32 len ^ str in
  Logs.debug (fun m -> m "write in total %d" len);
  let n = Unix2.write fd (Bytes.of_string str) 0 len in
  if n <> len
  then failwith "write error in 9P response";
  ()
