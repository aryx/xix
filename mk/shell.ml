(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

let shellpath = "/bin/sh"
(* -I for rc *)
let shellflags = []

let execsh shellenv flags inputs =

  let pid = Unix.fork () in
  
  (* children case *)
  if pid = 0
  then begin
    let (pipe_read, pipe_write) = Unix.pipe () in

    let pid2 = Unix.fork () in

    (* child 1, the shell interpeter, the process with pid returned by execsh *)
    if pid2 <> 0
    then begin
      Unix.dup2 pipe_read Unix.stdin;
      Unix.close pipe_read;
      Unix.close pipe_write;

      (try 
         let env = 
           shellenv |> List.map (fun (s, xs) -> 
             spf "%s=%s" s (String.concat " " xs))
         in
         Unix.execve 
           shellpath 
           (Array.of_list (flags @ shellflags))
           (Array.of_list env)
          |> ignore;
      with Unix.Unix_error _ -> failwith "Could not execute a shell command"
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
