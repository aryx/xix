(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*
let shellpath = "/bin/sh"
let shellflags = []
let iws = " "
*)
let shellpath = "/bin/rc"
let shellflags = ["-I"] (* non interactive, so? todo: no prompt? *)
let iws = "\001"


let execsh shellenv flags inputs interactive =

  let pid = Unix.fork () in
  
  (* children case *)
  if pid = 0
  then
    let env = 
      shellenv 
       (* bug: I exclude empty variables
        * otherwise rc does strange things. Indeed, programs
        * such as ocamlc get confused by empty variables
        * used in shell commands such as ocamlc $FLAG where FLAG is empty.
        * I get the problem also with mk-plan9port.
        * Note however that there is no problem with mk-sh.byte, so
        * this is an rc issue.
        *)
       |> List_.exclude (fun (s, xs) -> xs = [])

       |> List.map (fun (s, xs) -> spf "%s=%s" s (String.concat iws xs))
    in

    (* pad: I added this feature so mk can call interactive program
     * such as syncweb. Otherwise stdin is used to feed the shell
     * and so any program called from the shell will not have any stdin
     *)
    if interactive 
    then begin
      let tmpfile = Filename.temp_file "mk" "sh" in
      (try
         let chan = open_out tmpfile in
         inputs |> List.iter (fun s -> 
           output_string chan s; 
           output_string chan "\n"
         );
         close_out chan
       with Sys_error s -> 
         failwith (spf "Could not create temporary file (error = %s)" s)
      ); 
      (try 
         Unix.execve 
           shellpath 
           (Array.of_list (flags @ shellflags @ [tmpfile]))
           (Array.of_list env)
          |> ignore;
      with Unix.Unix_error (err, fm, argm) -> 
        if not (Sys.file_exists shellpath)
        then failwith (spf "could not find shell %s" shellpath)
        else failwith (spf "Could not execute a shell command: %s %s %s"
                         (Unix.error_message err) fm argm)
      );
      (* unreachable *)
      exit (-2);
      
    end else begin

    let (pipe_read, pipe_write) = Unix.pipe () in

    let pid2 = Unix.fork () in

    (* child 1, the shell interpeter, the process with pid returned by execsh *)
    if pid2 <> 0
    then begin
      Unix.dup2 pipe_read Unix.stdin;
      Unix.close pipe_read;
      Unix.close pipe_write;

      (try 
         Unix.execve 
           shellpath 
           (Array.of_list (flags @ shellflags))
           (Array.of_list env)
          |> ignore;
      with Unix.Unix_error (err, fm, argm) -> 
        if not (Sys.file_exists shellpath)
        then failwith (spf "could not find shell %s" shellpath)
        else failwith (spf "Could not execute a shell command: %s %s %s"
                         (Unix.error_message err) fm argm)
      );
      (* unreachable *)
      exit (-2);

    (* child 2, feeding the shell with inputs through a pipe *)
    end else begin
      Unix.close pipe_read;
      inputs |> List.iter (fun str ->
        let n = Unix.write pipe_write str 0 (String.length str) in
        if n < 0
        then failwith "Could not write in pipe to shell";
        let n = Unix.write pipe_write "\n" 0 1 in
        if n < 0
        then failwith "Could not write in pipe to shell";
      );
      (* will flush *)
      Unix.close pipe_write;
      exit 0;
    end
  end

  (* parent case *)
  else pid (* pid of child1 *)
