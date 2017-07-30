(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let author = ref ""
let committer = ref ""
let message = ref ""

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
      (* todo: allow cmd from different location *)
      let r = Repository.open_ "." in
      let today = 
        (Int64.of_float (Unix.time ()),
         { User.
           (* todo: use localtime vs gmtime? *)
           sign = User.Minus;
           hours = 7; (* SF *)
           min = 0;
         })
      in
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
      let committer =
        if !committer = ""
        then author
        else raise Todo
      in
      (* todo: imitate git output
       *   [master 0b50159] xxx
       *   1 file changed, 0 insertions(+), 0 deletions(-)
       *   create mode 100644 foobar.txt
       *)
      (* todo: nothing to commit, working directory clean *)
      Repository.commit_index r author committer !message
    | xs -> raise Cmd.ShowUsage
  );
}
