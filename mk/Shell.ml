(*s: mk/Shell.ml *)
(* Copyright 2016, 2018 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*s: type [[Shell.caps]] *)
(* Need:
 *  - exec/fork/wait: obviously as we run a shell
 *  - env: for MKSHELL
 *)
type caps = < Cap.exec; Cap.fork; Cap.wait; Cap.env >
(*e: type [[Shell.caps]] *)

(*s: type [[Shell.t]] *)
type t = { 
  path: Fpath.t;
  name: string;
  flags: string list;
  (* environment word separator *)
  iws: string;
  debug_flags: unit -> string list;
  (* less: in theory the escaping and quoting rules are different between
   * shells, so this should be part of the interface.
   *)
}
(*e: type [[Shell.t]] *)

(*s: constant [[Shell.sh]] *)
let sh = {
  path = Fpath.v "/bin/sh";
  name = "sh";
  flags = [];
  iws = " ";
  debug_flags = (fun () -> []);
}
(*e: constant [[Shell.sh]] *)

(* Should we pass -e too here to abort if error inside? This is done in
 * Scheduler.ml instead when executing recipe (but not for backquote processing)
 *)
(*s: constant [[Shell.rc]] *)
let rc = {
  path = Fpath.v "/usr/bin/rc";
  name = "rc";
  flags = ["-I"]; (* non-interactive so does not display a prompt *)
  iws = "\001";
  debug_flags = (fun () -> (* if !Flags.verbose then ["-v"] else [] *) []);
}
(*e: constant [[Shell.rc]] *)

(*s: function [[Shell.shell_from_env_opt]] *)
(* old: this is a toplevel entity, so the code below is executed even
 * before main, so you can not rely on the value in Flags as they have
 * not been set yet.
 * update: we now use capabilities so we could rely on Flags now
 * less: could use lazy to avoid recompute each time
 *)
let shell_from_env_opt (caps : < Cap.env; .. >) : t option = 
  try 
    let path = CapSys.getenv caps "MKSHELL" in
    match path with
    | s when s =~ ".*/rc$" -> Some { rc with path = Fpath.v path }
    | _ -> Some { sh with path = Fpath.v path }
  with Not_found -> None
(*e: function [[Shell.shell_from_env_opt]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Shell.exec_shell]] *)
let exec_shell (caps : < Cap.exec; Cap.env; ..>) shellenv flags extra_params =
  let shell = shell_from_env_opt caps ||| sh in
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
    |> List_.exclude (fun (_s, xs) -> xs = [])
    |> List.map (fun (s, xs) -> spf "%s=%s" s (String.concat shell.iws xs))
  in
  let args = flags @ shell.flags @ shell.debug_flags() @extra_params in
  let shell_path = !!(shell.path) in
  Logs.info (fun m -> m "exec_shell: %s %s" shell_path (String.concat " " args));
  (try 
     (* to debug pass instead "/usr/bin/strace" 
        (Array.of_list ("strace"::shell.path::args)) *)
     CapUnix.execve caps
       (* bugfix: need to pass shell.name too! otherwise the first elt
        * in args (usually '-e') will be taken for the prog name and skipped
        * by the shell (and mk will not stop at the first error)
        *)
       shell_path (Array.of_list (shell.name::args))
       (Array.of_list env)
      |> ignore;
   with Unix.Unix_error (err, fm, argm) -> 
     if not (Sys.file_exists shell_path)
     then failwith (spf "could not find shell %s" shell_path)
     else failwith (spf "Could not execute a shell command: %s %s %s"
                      (Unix.error_message err) fm argm)
  );
  (* nosemgrep: do-not-use-exit (unreachable) *)
  exit (-2)
(*e: function [[Shell.exec_shell]] *)

(*s: function [[Shell.feed_shell_input]] *)
let feed_shell_input inputs pipe_write =
  inputs |> List.iter (fun str ->
    let n = Unix.write pipe_write (Bytes.of_string str) 0 (String.length str) in
    if n < 0
    then failwith "Could not write in pipe to shell";
    let n = Unix.write pipe_write (Bytes.of_string "\n") 0 1 in
    if n < 0
    then failwith "Could not write in pipe to shell";
  );
  (* will flush *)
  Unix.close pipe_write
(*e: function [[Shell.feed_shell_input]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Shell.exec_recipe]] *)
(* returns a pid *)
let exec_recipe (caps : < Cap.fork; Cap.exec; .. >) (shellenv : Shellenv.t) flags inputs (interactive : bool) : int =
  let pid = CapUnix.fork caps () in
  
  (* children case *)
  if pid = 0
  then
    (*s: [[Shell.exec_recipe]] when children case, if [[interactive]] *)
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
      exec_shell caps shellenv flags [tmpfile]
      (* less: delete tmpfile *)
    (* unreachable *)
    end 
    (*e: [[Shell.exec_recipe]] when children case, if [[interactive]] *)
    else begin

    let (pipe_read, pipe_write) = Unix.pipe () in
    let pid2 = CapUnix.fork caps () in

    (* child 1, the shell interpeter, the process with pid returned by execsh *)
    if pid2 <> 0
    then begin
      Unix.dup2 pipe_read Unix.stdin;
      Unix.close pipe_read;
      Unix.close pipe_write;
      exec_shell caps shellenv flags []
      (* unreachable *)

    (* child 2, feeding the shell with inputs through a pipe *)
    end else begin
      Unix.close pipe_read;
      feed_shell_input inputs pipe_write;
      (* nosemgrep: do-not-use-exit (dont want to require caps for this one) *)
      exit 0;
    end
  end

  (* parent case *)
  else pid (* pid of child1 *)
(*e: function [[Shell.exec_recipe]] *)

(*s: function [[Shell.exec_backquote]] *)
let exec_backquote (caps : < caps; ..>) (shellenv : Shellenv.t) input =
  let (pipe_read_input, pipe_write_input)   = Unix.pipe () in
  let (pipe_read_output, pipe_write_output) = Unix.pipe () in

  let pid = CapUnix.fork caps () in

  (* child case *)
  if pid = 0
  then begin
    Unix.dup2 pipe_read_input Unix.stdin;
    Unix.dup2 pipe_write_output Unix.stdout;
    Unix.close pipe_read_input;
    Unix.close pipe_write_input;
    Unix.close pipe_read_output;
    Unix.close pipe_write_output;

    exec_shell caps shellenv [] [] 
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
        let s = Bytes.sub_string buffer 0 n in
        s ^ loop_read ()
    in
    let output = loop_read () in
    Unix.waitpid [] pid |> ignore;
    output
  end
(*e: function [[Shell.exec_backquote]] *)

(*s: function [[Shell.exec_pipecmd]] *)
let exec_pipecmd (caps: < caps; .. >) (shellenv : Shellenv.t) input =
  let tmpfile = Filename.temp_file "mk" "sh" in
  let (pipe_read_input, pipe_write_input)   = Unix.pipe () in

  let pid = CapUnix.fork caps () in

  (* child case *)
  if pid = 0
  then begin
    Unix.dup2 pipe_read_input Unix.stdin;
    Unix.close pipe_read_input;
    Unix.close pipe_write_input;
    let fd = Unix.openfile tmpfile [Unix.O_WRONLY] 0o640 in
    Unix.dup2 fd Unix.stdout;
    Unix.close fd;

    exec_shell caps shellenv [] [];
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
(*e: function [[Shell.exec_pipecmd]] *)
(*e: mk/Shell.ml *)
