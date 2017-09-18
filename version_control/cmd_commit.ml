(*s: version_control/cmd_commit.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function Cmd_commit.commit *)
let commit r author committer message =
  (* todo: imitate git output
   *   [master 0b50159] xxx
   *   1 file changed, 0 insertions(+), 0 deletions(-)
   *   create mode 100644 foobar.txt
   *)
  (* todo: nothing to commit, working directory clean *)
  Repository.commit_index r author committer message
(*e: function Cmd_commit.commit *)

(*s: constant Cmd_commit.author *)
let author = ref ""
(*e: constant Cmd_commit.author *)
(*s: constant Cmd_commit.committer *)
let committer = ref ""
(*e: constant Cmd_commit.committer *)
(*s: constant Cmd_commit.message *)
let message = ref ""
(*e: constant Cmd_commit.message *)

(*s: constant Cmd_commit.cmd *)
let cmd = { Cmd.
  name = "commit";
  help = " [options]"; (* less: <pathspec>... *)
  options = [
    "-m",        Arg.Set_string message, " commit message";
    "--message", Arg.Set_string message, " commit message";
    "--author", Arg.Set_string author, " <author> override author";
    "--committer", Arg.Set_string author, " ";
    (* less: commit mesg option: --file, --date, --signoff *)
    (* less: commit content options: -a, --interactive, --patch *)
    (* todo: --amend *)
  ];
  f = (fun args ->
    match args with
    | [] -> 
      let r, _ = Repository.find_root_open_and_adjust_paths [] in
      (*s: [[Cmd_commit.cmd]] compute [[today]] *)
      let today = 
        (Int64.of_float (Unix.time ()),
         { User.
       (* todo: use localtime vs gmtime? *)
           sign = User.Minus;
           hours = 7; (* SF *)
           min = 0;
         })
      in
      (*e: [[Cmd_commit.cmd]] compute [[today]] *)
      (*s: [[Cmd_commit.cmd]] compute [[author]] *)
      (* todo: read from .git/config or ~/.gitconfig *)
      let author = 
        if !author = ""
        then { User.
               name = Unix.getlogin ();
               email = "todo@todo";
               date = today;
             }
        else raise Todo (* need parse author string *)
      in
      (*e: [[Cmd_commit.cmd]] compute [[author]] *)
      (*s: [[Cmd_commit.cmd]] compute [[comitter]] *)
      let committer =
        if !committer = ""
        then author
        else raise Todo
      in
      (*e: [[Cmd_commit.cmd]] compute [[comitter]] *)
      commit r author committer !message

    | xs -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_commit.cmd *)
(*e: version_control/cmd_commit.ml *)
