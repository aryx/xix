(*s: Fileserver.mli *)
(* ?? *)

(*s: type [[Fileserver.t]] *)
type t = {
  (* the pipe *)

  (* clients_fd will be shared by all the winshell processes *)
  clients_fd: Unix.file_descr;
  server_fd: Unix.file_descr;

  (* for security *)
  user: string;

  (* the files managed by the server currently-in-use by the client *)
  fids: (File.fid, File.t) Hashtbl.t;

  (*s: [[Fileserver.t]] other fields *)
  (* refined after Tversion first message *)
  mutable message_size: int;
  (*e: [[Fileserver.t]] other fields *)
}
(*e: type [[Fileserver.t]] *)

(*s: signature [[Fileserver.init]] *)
(* internally creates a pipe between clients_fd/server_fd above *)
val init: unit -> t
(*e: signature [[Fileserver.init]] *)
(*e: Fileserver.mli *)
