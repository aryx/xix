(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of dulwich, itself a Python port of git, 
 * a distributed version control system.
 *
 * Main limitations compared to dulwich/git:
 *  -??
 * 
 * todo:
 *  - ??
 * 
 * The code of ogit uses code from 
 *  - ocaml-git
 *  - ocaml-hex
 *  - camlzip
 *  - extlib
 *  - uuidm
 *  - a few other sources.
 * 
 * Compared to ocaml-git, ogit is simpler because:
 *  - no use of functor, include, module types. KISS.
 *  - no functorized Set and Map so no need to hash, compare, and equal 
 *    boilerplate functions everywhere
 *  - hardcoded use of SHA1, so no need functors taking Git.DIGEST
 *  - no support for mirage, so no need to parametrize many things,
 *    no need Fs module, no need lwt
 *  - no disk vs mem, just disk, so again need less functors
 *  - hardcoded use of zlib
 *  - no support for filename requiring special escapes
 *  - no use of polymorphic variants
 *  - no use of keyword arguments, or default arguments
 *  - no use of Cstruct or Mstruct or Bigarray (simply use IO.ml and Bytes)
 *  - no logs
 *  - no fmt, just dump_gen or ocamldebug
 *  - no sexplib
 * 
 * good stuff I took from ocaml-git:
 *  - dotgit (more readable than commondir in dulwich)
 *  - '/' operator (more readable than all those os.path.join in dulwich)
 *  - Hash.Tree.t, Hash.Commit.t, Hash.Blob.t precise hash type?
 *  - TODO GRI (generalization of URI)
 *)

let commands = [
  Cmd_test.cmd;
  Cmd_dump.cmd;
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
      (* less: look if --help and factorize treatment of usage for subcmds *)
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
