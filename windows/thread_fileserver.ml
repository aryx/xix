open Common
open Fs

module Unix2 = ThreadUnix
module R = Worker_request

let dispatch fs fcall fid =
  raise Todo

(* the master *)
let thread fs =
  (* less: threadsetname *)
  
  while true do
    (* less: + IOHDRSZ + UTFmax *)
    let bufsize = 8192 in
    let buf = String.make bufsize ' ' in
    let n = Ninep.read fs.server_fd buf in

    if n <= 0
    then begin
      (* todo: should exit the whole proc *)
      failwith (spf "Ninep.read %d" n);
    end;
    
    let fcall = Ninep.parse buf n in
    (* less: dump fcall if debug *)

    let fid = Fid.alloc fcall.Ninep.fid in
    dispatch fs fcall fid
  done
