(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of dulwich, itself a Python port of git (the version control
 * system).
 *
 * Main limitations compared to dulwich/git:
 *  -??
 * 
 * todo:
 * 
 * The code of ogit uses code from 
 *  - ocaml-git
 *  - ocaml-hex
 *  - decompress
 *  - a few other sources.
 * 
 * Compared to ocaml-git, ogit is simpler because:
 *  - no use of functor
 *  - no use of polymorphic variants
 *  - no use of keyword arguments
 *  - no support for mirage, so no need to parametrize many things
 *  - hardcoded use of SHA1, so no need functors taking Git.DIGEST
 *  - hardcoded use of zlib
 *  - no use of Cstruct or Mstruct or Bigarray (simply use String)
 *  - no use of lwt
 *  - no logs
 *  - no pp
 *  - ...
 *)

let commands = [
  Cmd_test.cmd;
]
let hcommands = 
  commands |> List.map (fun cmd -> cmd.Cmd.name, cmd) |> Hashtbl_.of_list

let usage () =
  spf "usage: ogit <%s> [options]"
    (String.concat "|" (commands |> List.map (fun cmd -> cmd.Cmd.name)))

let main () =
  if Array.length Sys.argv < 2
  then begin
    pr2 (usage ());
    exit (-1);
  end
  else
    try 
      let cmd = Hashtbl.find hcommands Sys.argv.(1) in
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv -1) in
      let usage_msg_cmd = spf "%s [options]" cmd.Cmd.name in
      let remaining_args = ref [] in
      Arg.parse_argv argv (Arg.align cmd.Cmd.options) 
        (fun arg -> Common.push arg remaining_args) usage_msg_cmd;
      cmd.Cmd.f (List.rev !remaining_args)
    with 
      | Not_found ->
        pr2 (usage ());
        exit (-1);
      | Arg.Bad str ->
        failwith str
        

let _ =
  main ()
