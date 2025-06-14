(*s: shell/Builtin.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module R = Runtime
module E = Error
module O = Opcode

(*s: function [[Builtin.is_builtin]] *)
let is_builtin s =
  List.mem s [
    "cd"; 
    "."; 
    "eval";
    "exit";
    "flag";
    "finit";
    (* new in ocaml *)
    "show";
  ]
(*e: function [[Builtin.is_builtin]] *)
(*s: constant [[Builtin.ndots]] *)
let ndots = ref 0
(*e: constant [[Builtin.ndots]] *)
(*s: function [[Builtin.dochdir]] *)
let dochdir (caps : < Cap.chdir; .. >) s =
  try 
    Logs.info (fun m -> m "about to chdir to %s" s);
    CapUnix.chdir caps s;
    true
  with Unix.Unix_error _ ->
    false
(*e: function [[Builtin.dochdir]] *)
(*s: constant [[Builtin.dotcmds]] *)
(* for the builtin '.' (called 'source' in bash) *)
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
(*e: constant [[Builtin.dotcmds]] *)
(*s: function [[Builtin.dispatch]] *)
let dispatch (caps : < Cap.chdir; Cap.exit; Cap.open_in; ..>) s =
  match s with
  (*s: [[Builtin.dispatch()]] match [[s]] cases *)
  | "cd" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      (* default value in case something goes wrong below *)
      Status.setstatus "can't cd";

      (* less: cdpath vlook *)
      (match argv with
      | [_cd] -> 
          let v = (Var.vlook "home").R.v in
          (match v with
          | Some (dir::_) ->
            if dochdir caps dir
            then Status.setstatus ""
            (* less: %r *)
            else Logs.err (fun m -> m "Can't cd %s" dir)
          | _ -> Logs.err (fun m -> m "Can't cd -- $home empty")
          )
      | [_cd;dir] ->
          (* less: cdpath iteration *)
          if dochdir caps dir
          then Status.setstatus ""
          else Logs.err (fun m -> m "Can't cd %s" dir)
      | _ ->
          Logs.err (fun m -> m "Usage: cd [directory]");
      );
      R.pop_list()
  (*x: [[Builtin.dispatch()]] match [[s]] cases *)
  | "." ->
      let t = R.cur () in
      R.pop_word (); (* "." *)

      if !ndots > 0
      then Globals.eflagok := true;
      incr ndots;

      let iflag =
        match t.R.argv with
        | "-i"::_xs -> R.pop_word (); true
        | _ -> false
      in
      (match t.R.argv with
      | [] -> E.error caps "Usage: . [-i] file [arg ...]"
      | zero::args ->
          R.pop_word ();
          (* less: searchpath, also for dot? seems wrong *)
          (try 
            let file = zero in
            Logs.info (fun m -> m "evaluating %s" file);
            let chan = CapStdlib.open_in caps file in
            let newt = R.mk_thread dotcmds 0 (Hashtbl.create 10) in
            R.runq := [newt];
            R.push_redir (R.Close (Unix.descr_of_in_channel chan));
            newt.R.file <- Some (Fpath.v file);
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
            E.error caps ".: can't open"
          )
      ) 
  (*x: [[Builtin.dispatch()]] match [[s]] cases *)
  | "flag" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [_flag;letter] ->
          (* stricter: *)
          if String.length letter <> 1
          then E.error caps "flag argument must be a single letter"
          else begin
            let char = String.get letter 0 in
            let is_set = Hashtbl.mem Flags.hflags char in
            Status.setstatus (if is_set then "" else "flag not set");
          end

      | [_flag;_letter;_set] ->
          failwith "TODO: flag letter +- not handled yet"

      | _ -> E.error caps ("Usage: flag [letter] [+-]")
      );
      R.pop_list()

  (*x: [[Builtin.dispatch()]] match [[s]] cases *)
  | "finit" -> 
     (* less: Xrdfn *)
     R.pop_list ()
  (*e: [[Builtin.dispatch()]] match [[s]] cases *)
  | "show" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [_show;"vars"] ->
          Logs.app (fun m -> m "--- GLOBALS ---");
          R.globals |> Hashtbl_.to_list |> Assoc.sort_by_key_lowfirst |>
          List.iter (fun (k, v) ->
              Logs.app (fun m -> m "%s=%s" k (Runtime.string_of_var v))
          );
          Logs.app (fun m -> m "--- LOCALS ---");
          t.R.locals |> Hashtbl.iter (fun k v ->
              Logs.app (fun m -> m "%s=%s" k (Runtime.string_of_var v))
          );
          Logs.app (fun m -> m "--- FUNCTIONS ---");
          R.fns |> Hashtbl.iter (fun k fn ->
              Logs.app (fun m -> m "%s=%s" k (Runtime.show_fn fn))
          );
          ()
      | _ -> E.error caps ("Usage: show (vars|thread)")
      );


  | _ -> failwith (spf "unsupported builtin %s" s)
(*e: function [[Builtin.dispatch]] *)
(*e: shell/Builtin.ml *)
