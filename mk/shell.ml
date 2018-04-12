(* Copyright 2016, 2018 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type t = { 
  path: Common.filename;
  flags: string list;
  (* environment word separator *)
  iws: string;
  debug_flags: unit -> string list;
  (* less: in theory the escaping and quoting rules are different between
   * shells, so this should be part of the interface.
   *)
}

let sh = {
  path = "/bin/sh";
  flags = [];
  iws = " ";
  debug_flags = (fun () -> []);
}

let rc = {
  path = "/usr/bin/rc";
  flags = ["-I"]; (* non-interactive so does not display a prompt *)
  iws = "\001";
  debug_flags = (fun () -> if !Flags.verbose then ["-v"] else []);
}

(* note that this is a toplevel entity, so the code below is executed even
 * before main, so you can not rely on the value in Flags as they have
 * not been set yet.
 *)
let shell = 
  try 
    let path = Sys.getenv "MKSHELL" in
    match path with
    | s when s =~ ".*/rc$" -> { rc with path = path }
    | _ -> { sh with path = path }
  with Not_found -> sh

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let exec_shell shellenv flags extra_params =
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

    |> List.map (fun (s, xs) -> spf "%s=%s" s (String.concat shell.iws xs))
  in
  (try 
     Unix.execve 
       shell.path 
       (Array.of_list (flags @ shell.flags @ shell.debug_flags() @extra_params))
       (Array.of_list env)
      |> ignore;
   with Unix.Unix_error (err, fm, argm) -> 
     if not (Sys.file_exists shell.path)
     then failwith (spf "could not find shell %s" shell.path)
     else failwith (spf "Could not execute a shell command: %s %s %s"
                      (Unix.error_message err) fm argm)
  );
  (* unreachable *)
  exit (-2)

let feed_shell_input inputs pipe_write =
  inputs |> List.iter (fun str ->
    let n = Unix.write pipe_write str 0 (String.length str) in
    if n < 0
    then failwith "Could not write in pipe to shell";
    let n = Unix.write pipe_write "\n" 0 1 in
    if n < 0
    then failwith "Could not write in pipe to shell";
  );
  (* will flush *)
  Unix.close pipe_write


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let exec_recipe shellenv flags inputs interactive =
  let pid = Unix.fork () in
  
  (* children case *)
  if pid = 0
  then

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
      exec_shell shellenv flags [tmpfile]
    (* unreachable *)
    end else begin

    let (pipe_read, pipe_write) = Unix.pipe () in
    let pid2 = Unix.fork () in

    (* child 1, the shell interpeter, the process with pid returned by execsh *)
    if pid2 <> 0
    then begin
      Unix.dup2 pipe_read Unix.stdin;
      Unix.close pipe_read;
      Unix.close pipe_write;
      exec_shell shellenv flags []
      (* unreachable *)

    (* child 2, feeding the shell with inputs through a pipe *)
    end else begin
      Unix.close pipe_read;
      feed_shell_input inputs pipe_write;
      exit 0;
    end
  end

  (* parent case *)
  else pid (* pid of child1 *)


let exec_backquote shellenv input =
  let (pipe_read_input, pipe_write_input)   = Unix.pipe () in
  let (pipe_read_output, pipe_write_output) = Unix.pipe () in

  let pid = Unix.fork () in

  (* child case *)
  if pid = 0
  then begin
    Unix.dup2 pipe_read_input Unix.stdin;
    Unix.dup2 pipe_write_output Unix.stdout;
    Unix.close pipe_read_input;
    Unix.close pipe_write_input;
    Unix.close pipe_read_output;
    Unix.close pipe_write_output;

    exec_shell shellenv [] [] 
    (* unreachable *)
  end else begin
    (* parent case *)

    Unix.close pipe_read_input;
    Unix.close pipe_write_output;

    feed_shell_input [input] pipe_write_input;

    (* read the shell output through the other pipe *)
    let buffer = Bytes.create 1024 in
    let rec loop_read () =
      let n = Unix.read pipe_read_output buffer 0 1024 in
      match n with
      | 0 -> ""
      | _ when n < 0 ->
        failwith "Could not read from pipe to shell";
      | _ -> 
        let s = String.sub buffer 0 n in
        s ^ loop_read ()
    in
    let output = loop_read () in
    Unix.waitpid [] pid |> ignore;
    output
  end


let exec_pipecmd shellenv input =
  let tmpfile = Filename.temp_file "mk" "sh" in
  let (pipe_read_input, pipe_write_input)   = Unix.pipe () in

  let pid = Unix.fork () in

  (* child case *)
  if pid = 0
  then begin
    Unix.dup2 pipe_read_input Unix.stdin;
    Unix.close pipe_read_input;
    Unix.close pipe_write_input;
    let fd = Unix.openfile tmpfile [Unix.O_WRONLY] 0o640 in
    Unix.dup2 fd Unix.stdout;
    Unix.close fd;

    exec_shell shellenv [] [];
    (* unreachable *)
  end else begin
    (* parent case *)

    Unix.close pipe_read_input;
    
    feed_shell_input [input] pipe_write_input;

    let (pid2, status) = Unix.waitpid [] pid in
    if pid <> pid2
    then raise (Impossible "waitpid takes the specific pid");
    (match status with
    | Unix.WEXITED 0 -> tmpfile
    (* stricter: fail fast, no "warning: skipping missing program file: " *)
    | _ -> failwith "bad include program status"
    )
  end
