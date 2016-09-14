open Common

module R = Runtime
module E = Error
module O = Opcode

let is_builtin s =
  List.mem s [
    "cd"; 
    "."; "eval"; 
    "exit";
    "flag";
    "finit"
  ]

let dochdir s =
  try 
    Unix.chdir s;
    true
  with Unix.Unix_error _ ->
    false

(*  *)
let dotcmds = 
  [|
    O.F O.Mark;
      O.F O.Word;
      O.S "0";
    O.F O.Local;

    O.F O.Mark;
      O.F O.Word;
      O.S "*";
    O.F O.Local;

    O.F O.REPL;

    O.F O.Unlocal;
    O.F O.Unlocal;
    O.F O.Return;
  |]         

let dispatch s =
  match s with
  | "cd" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      Status.setstatus "can't cd";

      (* less: cdpath vlook *)
      (match argv with
      | [cd] -> 
          let v = (Var.vlook "home").R.v in
          (match v with
          | Some (dir::_) ->
            if dochdir dir
            then Status.setstatus ""
            (* less: %r *)
            else pr2 (spf "Can't cd %s" dir)
          | _ -> pr2 ("Can't cd -- $home empty")
          )
      | [_cd;dir] ->
          (* less: cdpath iteration *)
          if dochdir dir
          then Status.setstatus ""
          else pr2 (spf "Can't cd %s" dir)
      | _ ->
          pr2 "Usage: cd [directory]";
      );
      R.pop_list()

  | "." ->
      let t = R.cur () in
      R.pop_word (); (* "." *)
      (* less: eflagok to reset it when executed first *)

      let iflag =
        match t.R.argv with
        | "-i"::xs -> R.pop_word (); true
        | _ -> false
      in
      (match t.R.argv with
      | [] -> E.error "Usage: . [-i] file [arg ...]"
      | zero::args ->
          R.pop_word ();
          (* less: searchpath, also for dot? seems wrong *)
          (try 
            let file = zero in
            let chan = open_in file in
            let newt = R.mk_thread dotcmds 0 (Hashtbl.create 10) in
            R.runq := [newt];
            R.push_redir (R.Close (Unix.descr_of_in_channel chan));
            newt.R.file <- Some file;
            newt.R.lexbuf <- Lexing.from_channel chan;
            newt.R.iflag <- iflag;
            (* push for $* *)
            R.push_list ();
            newt.R.argv <- args;
            (* push for $0 *)
            R.push_list ();
            newt.R.argv <- [zero];

          with Failure _ ->
            prerr_string (spf "%s: " zero);
            E.error ".: can't open"
          )
      ) 

  | "flag" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [_flag;letter] ->
          (* stricter: *)
          if String.length letter <> 1
          then E.error "flag argument must be a single letter"
          else begin
            let char = String.get letter 0 in
            let is_set =
              try Hashtbl.find Flags.hflags char
              with Not_found -> false
            in
            Status.setstatus (if is_set then "" else "flag not set");
          end

      | [_flag;letter;set] ->
          failwith "TODO: flag letter +- not handled yet"

      | _ -> E.error ("Usage: flag [letter] [+-]")
      );
      R.pop_list()

  | "finit" -> 
     (* less: Xrdfn *)
     R.pop_list ()

  | _ -> failwith (spf "unsupported builtin %s" s)

