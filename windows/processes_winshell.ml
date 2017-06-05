open Common

module Unix1 = Unix
module Unix2 = ThreadUnix

module FS = Fileserver
module W = Window


let child_of_fork w fs cmd argv =
  Unix1.chdir w.W.pwd;
  (* todo: close on exec *)
  (* less: rfork for copy of namespace/fd/env, but ape fork does that? *)
    
  (*/* close server end so mount won't hang if exiting */*)
  Unix1.close fs.FS.server_fd;
  Plan9.mount fs.FS.clients_fd (-1) "/mnt/wsys" Plan9.MRepl (spf "%d" w.W.id);
  Plan9.bind "/mnt/wsys" "/dev" Plan9.MBefore;

  (* less: wclose for ref counting *)
  (* todo: reassign STDIN/STDOUT *)
  (* less: notify nil *)
  Unix2.execv cmd argv;
  failwith "exec failed"
