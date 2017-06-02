open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix
  
(* Note that pipes created under plan9 are bidirectional! 
 * No need to create 2 pipes for 2-way communication.
 *)

type t = {
  (* the pipe *)

  (* clients_fd will be shared by all the winshell processes *)
  clients_fd: Unix1.file_descr;
  server_fd: Unix1.file_descr;

  (* for security *)
  user: string;

  fids: (Fid.fid, Fid.t) Hashtbl.t;
}

let init () =
  (* less: close on exec flag, or need to do it when exec in winshell *)
  let (fd1, fd2) = Unix2.pipe () in
  (* todo: default threadUnix implementation just set non_block for fd2, but
   * the in_fd, but in plan9 pipes are bidirectional so need non_block for
   * both file descriptors.
   *)
  Unix.set_nonblock fd1;

  
  (* less: let user = Common.cat "/dev/user" |> String.concat "" in *)
  
  { clients_fd = fd1;
    server_fd = fd2;

    user = "pad";
    fids = Hashtbl.create 101;
  }
