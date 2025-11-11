
let setup lvl () =
  (* no special setup needed for xix Logs.ml *)
  Logs.set_level lvl

let cli_flags (level : Logs.level option ref) : 
  (Arg.key * Arg.spec * Arg.doc) list = 
  [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " trace the main functions";
  ]
