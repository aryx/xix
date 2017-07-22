(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let author = ref ""
let committer = ref ""
let message = ref ""

let cmd = { Cmd.
  name = "commit";
  help = "";
  options = [
    "-m", Arg.Set_string message, " ";
    "--message", Arg.Set_string message, " ";
    "--author", Arg.Set_string author, " ";
    "--committer", Arg.Set_string author, " ";
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
      Repository.commit_index r author committer !message
    | xs -> 
      (* less: usage message? *)
      failwith "usage: git commit"
  );
}
