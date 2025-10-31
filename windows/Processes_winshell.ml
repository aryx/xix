(* Copyright 2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module Unix1 = Unix
module Unix2 = (*Thread*)Unix

let run_cmd_in_window_in_child_of_fork
  (caps : < Cap.chdir; Cap.exec; .. >)
  (cmd : string) (argv : string array) (w : Window.t) (fs : Fileserver.t) =
  (*Unix1*)CapUnix.chdir caps !!(w.pwd);
  (* todo? rfork for copy of namespace/fd/env, but ape fork does that? *)
    
  (*/* close server end so mount won't hang if exiting */*)
  Unix1.close fs.server_fd;
  (* todo: close on exec? *)

  Plan9.mount fs.clients_fd (-1) (Fpath.v "/mnt/wsys") Plan9.MRepl (spf "%d" w.id);
  Plan9.bind (Fpath.v "/mnt/wsys") (Fpath.v "/dev") Plan9.MBefore;

  (* less: wclose for ref counting *)
  (* todo: handle errors? Unix_error? then communicate failure to parent? *)
  (* bugfix: do not forget the last perm argument below! otherwise partial
   * application and stdin/stdout are never reopened (in fact the dup
   * below even crashes the plan9 kernel)
   *)
  Unix1.close Unix1.stdin;
  let _fd = Unix1.openfile "/dev/cons" [Unix1.O_RDONLY] 0o666 in
  Unix1.close Unix1.stdout;
  let _fd = Unix1.openfile "/dev/cons" [Unix1.O_WRONLY] 0o666 in
  Unix1.dup2 Unix1.stdout Unix1.stderr; 

  (* less: notify nil *)
  (*Unix2*)CapUnix.execv caps cmd argv |> ignore;
  (* should never reach this point *)
  failwith "exec failed"
