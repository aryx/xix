open Common

module Unix1 = Unix
module Unix2 = ThreadUnix

module FS = Fileserver
module W = Window


let run_cmd_in_window_in_child_of_fork cmd argv w fs =
  Unix1.chdir w.W.pwd;
  (* todo? rfork for copy of namespace/fd/env, but ape fork does that? *)
    
  (*/* close server end so mount won't hang if exiting */*)
  Unix1.close fs.FS.server_fd;
  (* todo: close on exec? *)

  Plan9.mount fs.FS.clients_fd (-1) "/mnt/wsys" Plan9.MRepl (spf "%d" w.W.id);
  Plan9.bind "/mnt/wsys" "/dev" Plan9.MBefore;

  (* less: wclose for ref counting *)
  (* todo: handle errors? Unix_error? then communicate failure to parent? *)
  (* bugfix: do not forget the last perm argument below! otherwise partial
   * application and stdin/stdout are never reopened (in fact the dup
   * below even crashes the plan9 kernel)
   *)
  Unix1.close Unix1.stdin;
  let fd = Unix1.openfile "/dev/cons" [Unix1.O_RDONLY] 0o666 in
  Unix1.close Unix1.stdout;
  let fd = Unix1.openfile "/dev/cons" [Unix1.O_WRONLY] 0o666 in
  Unix1.dup2 Unix1.stdout Unix1.stderr; 

  (* less: notify nil *)
  Unix2.execv cmd argv;
  (* should never reach this point *)
  failwith "exec failed"
