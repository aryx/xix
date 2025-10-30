open Common

type fid = int32
type tag = int16

module Request : sig
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

module Response : sig
  type t = 
    | Version of int (* message size *) * string (* "9P2000" *)
    | Attach of Plan9.qid

    | Error of string

    | Open of Plan9.qid * int (* iounit *)
    | Read of bytes (* data *) (* TODO: string? need modify in place? *)
    | Write of int (* count *)
    | Clunk

    | Walk of Plan9.qid list (* < max_welem *)
    | Create of Plan9.qid * int (* iounit *)
    | Remove
    | Stat of Plan9.dir_entry
    | Wstat

    | Flush
    | Auth of Plan9.qid (* auth_qid *)

end

type message_type =
  | T of Request.t
  | R of Response.t

type message = {
  tag: tag;
  typ: message_type;
}

val io_header_size: int
val max_welem: int

(* main interface *)
val read_9P_msg: 
  Unix.file_descr -> message
val write_9P_msg: 
  message -> Unix.file_descr -> unit

(* to debug *)
val str_of_msg: message -> string
