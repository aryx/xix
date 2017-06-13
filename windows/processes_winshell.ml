open Common

module Unix1 = Unix
module Unix2 = ThreadUnix

module FS = Fileserver
module W = Window


let run_cmd_in_window_in_child_of_fork cmd argv w fs =
  Unix1.chdir w.W.pwd;
  (* todo: close on exec *)
  (* less: rfork for copy of namespace/fd/env, but ape fork does that? *)
    
  (*/* close server end so mount won't hang if exiting */*)
  Unix1.close fs.FS.server_fd;

  Plan9.mount fs.FS.clients_fd (-1) "/mnt/wsys" Plan9.MRepl (spf "%d" w.W.id);
  Plan9.bind "/mnt/wsys" "/dev" Plan9.MBefore;

  (* less: wclose for ref counting *)
  (* todo: handle errors? Unix_error? then communicate failure to parent? *)
  Unix1.close Unix1.stdin;
  let fd = Unix1.openfile "/dev/cons" [Unix1.O_RDONLY] 0o666 in
  if fd <> Unix1.stdin
  then failwith "could not reassign stdin";

  Unix1.close Unix1.stdout;
  let fd = Unix1.openfile "/dev/cons" [Unix1.O_WRONLY] 0o666 in
  if fd <> Unix1.stdout
  then failwith "could not reassign stdout";
  
  (* todo: Unix1.dup2 Unix1.stdout Unix1.stderr; 
   * this creates a kernel crash! 'double sleep' error.
  *)
(*
  Unix1.close Unix1.stderr;
  let _ = Unix1.openfile "/dev/cons" [Unix1.O_WRONLY] 0o666 in
*)  

  (* less: notify nil *)
  Unix2.execv cmd argv;
  failwith "exec failed"
