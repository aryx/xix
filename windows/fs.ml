open Common

type t = {
  (* the pipe *)

  (* clients_fd will be shared by all the winshell processes *)
  clients_fd: Unix.file_descr;
  server_fd: Unix.file_descr;

  (* for security *)
  user: string;

  fids: (Fid.fid, Fid.t) Hashtbl.t;
}

let init () =
  (* less: close on exec flag, or need to do it when exec in winshell *)
  let (pipe_for_read, pipe_for_write) = Unix.pipe () in
  
  (* less: let user = Common.cat "/dev/user" |> String.concat "" in *)
  
  raise Todo
