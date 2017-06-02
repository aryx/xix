open Common
open Fileserver

module P = Protocol_9P

module Unix2 = ThreadUnix
module R = Worker_request

let dispatch fs req fid =
  raise Todo

(* the master *)
let thread fs =
  (* less: threadsetname *)
  
  while true do
    (* less: care about messagesize? *)
    let req = P.read_9P_msg fs.server_fd in

    (* todo: should exit the whole proc if error *)
    
    (* less: dump fcall if debug *)

    let fid = 
      (* Fid.alloc req.P.fid *)
      raise Todo
    in
    dispatch fs req fid
  done
