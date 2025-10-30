open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = (*Thread*)Unix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
  
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
  (* refined after Tversion first message *)
  mutable message_size: int;

  (* the files managed by the server currently-in-use by the client *)
  fids: (File.fid, File.t) Hashtbl.t;
}

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let init () =
  let (fd1, fd2) = Unix2.pipe () in
  (* the default threadUnix implementation just set non_block for fd2
   * (the 'in_fd'), but in plan9 pipes are bidirectional so we need 
   * to set non_block for fd1 too.
   *)
  Unix.set_nonblock fd1;
  (* todo? record fd2 as close_on exec? 
   * Unix.set_close_on_exec? but when it's useful really? just cleaner/safer?
   *)
 
  { clients_fd = fd1;
    server_fd = fd2;

    (* todo: let user = Common.cat "/dev/user" |> String.concat "" in *)
    user = "pad";
    message_size = 8192 + Protocol_9P.io_header_size;

    fids = Hashtbl.create 101;
  }
