(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of git, a distributed version control system.
 *
 * Some of the code of ogit derives from dulwich (a clone of git in Python)
 * and ocaml-git (a clone of git in OCaml).
 *
 * Main limitations compared to git/dulwich/ocaml-git:
 *  -??
 * 
 * todo:
 *  - a simplified version where just Marshall data instead of using
 *    the specific git format. Save many LOC?
 *  - ??
 * 
 * The code of ogit uses code from 
 *  - ocaml-git
 *  - ocaml-hex
 *  - camlzip
 *  - extlib
 *  - uuidm
 * 
 * ogit uses lots of code from ocaml-git. However, ogit is simpler because
 * it does not use fancy features of OCaml or fancy libraries:
 *  - no functor, include, module types, polymorphic variants, keyword args,
 *    or excessive nested modules. KISS.
 *  - no functorized Set and Map so no need for hash(), compare(), and equal()
 *    boilerplate functions everywhere
 *  - hardcoded use of SHA1, so no need functors taking Git.DIGEST and HashIO
 *  - no support for mirage, so no need to parametrize many things,
 *    no need Fs module, no need lwt
 *  - no disk vs mem, just disk, so again need less functors
 *  - hardcoded use of zlib so no need functors taking inflate signature
 *  - no support for filename requiring special escapes
 *  - no use of Cstruct or Mstruct or Bigarray (simply use IO.ml and Bytes)
 *  - no logs
 *  - no fmt (use ocamldebug or ocamltarzan dumpers)
 *  - no sexplib
 * 
 * good stuff I took from ocaml-git:
 *  - dotgit (more readable than commondir in dulwich)
 *  - '/' operator (more readable than all those os.path.join in dulwich)
 *  - Hash.Tree.t, Hash.Commit.t, Hash.Blob.t more precise hash types
 *    (but they are not statically checked in ogit)
 *  - TODO GRI (generalization of URI)
 * good stuff I wish I could take from dulwich:
 *  - hashtbl [] overloading, so can do r.refs["refs/tags/"+tag] = obj.id
 *    (thx to __setitem__ and __getitem__)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let commands = List.flatten [
  Cmds.main_commands;
  Cmds.extra_commands;
  [Cmd_help.cmd];
]

let hcommands = 
  commands |> List.map (fun cmd -> cmd.Cmd.name, cmd) |> Hashtbl_.of_list

let usage () =
  spf "usage: ogit <%s> [options]"
    (String.concat "|" (commands |> List.map (fun cmd -> cmd.Cmd.name)))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =
  if Array.length Sys.argv < 2
  then begin
    pr2 (usage ());
    exit 1;
  end
  else begin
    let cmd = 
      try 
        Hashtbl.find hcommands Sys.argv.(1) 
      with Not_found ->
        pr2 (usage ());
        exit 1
    in
    let argv = Array.sub Sys.argv 1 (Array.length Sys.argv -1) in
    let usage_msg_cmd = spf "%s %s [options]" 
      Sys.argv.(0)
      cmd.Cmd.name 
    in
    let remaining_args = ref [] in
    (try 
     (* todo: look if --help and factorize treatment of usage for subcmds *)
       Arg.parse_argv argv (Arg.align cmd.Cmd.options) 
         (fun arg -> Common.push arg remaining_args) usage_msg_cmd;
     with Arg.Bad str -> failwith str 
    );
    (* finally! *)
    try 
      cmd.Cmd.f (List.rev !remaining_args)
    with Cmd.ShowUsage ->
      Arg.usage (Arg.align cmd.Cmd.options) usage_msg_cmd;
      exit 1
  end
        
let _ =
  main ()
