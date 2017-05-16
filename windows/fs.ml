
type t = {
  (* the pipe *)

  (* clients_fd will be shared by all the winshell processes *)
  clients_fd: Unix.file_descr;
  server_fd: Unix.file_descr;

  (* for security *)
  user: string;

  fids: (Fid.fid, Fid.t) Hashtbl.t;
}
