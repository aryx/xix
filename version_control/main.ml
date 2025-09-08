(*s: version_control/main.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of git, a distributed version control system.
 *
 * Some of the code derives from dulwich (a clone of git in Python)
 * and ocaml-git (another clone of git in OCaml).
 *
 * Main limitations compared to git/dulwich/ocaml-git:
 *  -??
 * 
 * todo:
 *  - a simplified version where just Marshall data instead of using
 *    the specific git format. Save many LOC?
 *  - ??
 * 
 * The code of ocamlgit uses code from 
 *  - ocaml-git
 *  - ocaml-hex
 *  - camlzip
 *  - extlib
 *  - uuidm
 * 
 * ocamlgit uses code from ocaml-git. However, ocamlgit is simpler because
 * it does not use fancy features of OCaml or fancy libraries:
 *  - no functor, include, module types, polymorphic variants, keyword args,
 *    or excessive nested modules. KISS.
 *  - no functorized Set and Map so no need for hash(), compare(), and equal()
 *    boilerplate functions everywhere
 *  - hardcoded use of SHA1, so no need functors taking Git.DIGEST and HashIO
 *  - no support for Mirage, so no need to parametrize many things,
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
 *    (but they are not statically checked in ocamlgit)
 *  - TODO GRI (generalization of URI)
 * good stuff I wish I could take from dulwich:
 *  - hashtbl [] overloading, so can do r.refs["refs/tags/"+tag] = obj.id
 *    (thx to __setitem__ and __getitem__, but true that it also entails
 *     lots of boilerplate code)
 *
 * TODO:
 *  - look go-git, better basis? more complete than ocamlgit?
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: constant [[Main.commands]] *)
let commands = List.flatten [
  Cmds.main_commands;
  Cmds.extra_commands;
  [Cmd_help.cmd];
]
(*e: constant [[Main.commands]] *)

(*s: function [[Main.usage]] *)
let usage () =
  spf "usage: ocamlgit <%s> [options]"
    (String.concat "|" (commands |> List.map (fun cmd -> cmd.Cmd_.name)))
(*e: function [[Main.usage]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Main.main]] *)
let main () =
  (*s: [[Main.main()]] GC settings *)
  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (*e: [[Main.main()]] GC settings *)
  (*s: [[Main.main()]] sanity check arguments *)
  if Array.length Sys.argv < 2
  then begin
    (*s: [[Main.main()]] print usage and exit *)
    UConsole.print (usage ());
    exit 1
    (*e: [[Main.main()]] print usage and exit *)
  end
  (*e: [[Main.main()]] sanity check arguments *)
  else begin
    let cmd = 
      try 
        commands |> List.find (fun cmd -> cmd.Cmd_.name = Sys.argv.(1))
      with Not_found ->
        (*s: [[Main.main()]] print usage and exit *)
        UConsole.print (usage ());
        exit 1
        (*e: [[Main.main()]] print usage and exit *)
    in
    (*s: [[Main.main()]] execute [[cmd.f]] *)
    let argv = Array.sub Sys.argv 1 (Array.length Sys.argv -1) in
    let usage_msg_cmd = spf "usage: %s %s%s"
      (Filename.basename Sys.argv.(0))
      cmd.Cmd_.name
      cmd.Cmd_.usage
    in
    let remaining_args = ref [] in
    (*s: [[Main.main()]] parse [[argv]] for cmd options and [[remaining_args]] *)
    (try 
     (* todo: look if --help and factorize treatment of usage for subcmds *)
       Arg.parse_argv argv (Arg.align cmd.Cmd_.options) 
         (fun arg -> Stack_.push arg remaining_args) usage_msg_cmd;
     with Arg.Bad str | Arg.Help str->  
       prerr_string str;
       exit 1
    );
    (*e: [[Main.main()]] parse [[argv]] for cmd options and [[remaining_args]] *)
    (* finally! *)
    try 
      cmd.Cmd_.f (List.rev !remaining_args)
    with 
      | Cmd_.ShowUsage ->
        Arg.usage (Arg.align cmd.Cmd_.options) usage_msg_cmd;
        exit 1
    (*e: [[Main.main()]] execute [[cmd.f]] *)
  end
(*e: function [[Main.main]] *)
        
(*s: toplevel [[Main._1]] *)
let _ =
  main ()
(*e: toplevel [[Main._1]] *)
(*e: version_control/main.ml *)
