open Common

module O = Opcode
module R = Runtime
module E = Error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* could be in runtime.ml *)
let file_descr_of_int i =
  match i with
  | 0 -> Unix.stdin
  | 1 -> Unix.stdout
  | 2 -> Unix.stderr
  (* todo: how do that? if do >[1=4] ?? *)
  | n -> failwith (spf "file_descr_of_int: unsupported int %d" n)

let int_at_address t pc =
  match t.R.code.(pc) with
  | O.I i -> i
  (* stricter: generate error, but should never happen *)
  | op -> failwith (spf "was expecting I, not %s at %d" 
                      (Dumper.s_of_opcode op) pc)


let vlook_varname_or_index varname =
  if varname =~ "^[0-9]+$"
  then
    let i = int_of_string varname in
    let v = (Var.vlook "*").R.v in
    (match v with
    (* stricter: array out of bound checking *)
    | None -> failwith "undefined $*"
    | Some xs -> 
        (* list indexes in rc starts at 1, not 0 *)
        if i >= 1 && i <= List.length xs
        then Some ([List.nth xs (i-1)])
        else failwith (spf "out of bound, $%d too big for $*" i)
    )
  else (Var.vlook varname).R.v

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let interpret operation =
  match operation with
  (* *)
  | O.REPL -> Op_repl.op_REPL ()

  (* (args) *)
  | O.Simple -> Op_process.op_Simple ()

  | O.Return -> Process.return ()
  | O.Exit -> 
      (* todo: trapreq *)
      Process.exit (Status.getstatus())

  | O.Mark -> R.push_list ()

  (* [string] *)
  | O.Word ->
      let t = R.cur () in
      let pc = t.R.pc in
      let x = t.R.code.(!pc) in
      incr pc;
      (match x with
      | O.S s -> R.push_word s
      (* stricter: but should never happen *)
      | op -> failwith (spf "was expecting a S, not %s" (Dumper.s_of_opcode op))
      )

  (* (name) (val) *)
  | O.Assign ->
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [varname] ->
          (* no call to globlist for varname as it can be "*" for $* *)
          (* less: deglob varname *)
          let v = Var.vlook varname in
          R.pop_list ();

          (* less: globlist for the arguments *)
          let argv = t.R.argv in
          v.R.v <- Some argv;
          R.pop_list ();

      | _ -> E.error "variable name not singleton!"
      )

  (* (name) (val) *)
  | O.Local ->
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [varname] ->
        (* less: deglob varname *)
        R.pop_list ();
        (* less: globlist *)
        let argv = t.R.argv in
        Hashtbl.add t.R.locals varname { R.v = Some argv };
        R.pop_list ();
      | _ -> E.error "variable name not singleton!"
      )

  (* [i j]{... Xreturn}{... Xreturn} *)
  | O.Pipe -> 
      let t = R.cur () in
      let pc = t.R.pc in
      (* left file descriptor, should be stdout *)
      let lfd =
        let i = int_at_address t !pc in
        file_descr_of_int i
      in
      incr pc;
      (* right file descriptor, should be stdin *)
      let rfd =
        let i = int_at_address t !pc in
        file_descr_of_int i
      in
      incr pc;

      let (pipe_read, pipe_write) = Unix.pipe () in
      let forkid = Unix.fork () in

      (* child *)
      if forkid = 0 then begin
        (* less: clearwaitpids () *)
        (* pc + 2 to jump over the jump addresses *)
        let newt = R.mk_thread t.R.code (!pc + 2) t.R.locals in
        R.runq := [newt];
        Unix.close pipe_read;
        R.push_redir (R.FromTo (pipe_write, lfd));
      (* parent *)
      end else begin
        (* less: addwaitpid () *)
        let newt = 
          R.mk_thread t.R.code (int_at_address t (!pc+0)) t.R.locals in
        R.runq := newt::!R.runq;
        Unix.close pipe_write;
        R.push_redir (R.FromTo (pipe_read, rfd));
      
       (* once newt finished, jump to Xpipewait *)
        pc := int_at_address t (!pc+1);
        t.R.waitstatus <- R.WaitFor forkid;
      end

  (* argument passed through Thread.pid *)
  | O.PipeWait -> 
      let t = R.cur () in
      (match t.R.waitstatus with
      (* stricter: *)
      | R.NothingToWaitfor -> 
          failwith "Impossible: NothingToWaitfor for PipeWait"
      (* a previous waitfor() already got it *)
      | R.ChildStatus status -> 
          Status.setstatus (Status.concstatus status (Status.getstatus()));
      | R.WaitFor pid ->
          let status = Status.getstatus () in
          (* will internally call setstatus() when it found the right child *)
          Process.waitfor pid |> ignore;
          t.R.waitstatus <- R.NothingToWaitfor;
          Status.setstatus (Status.concstatus (Status.getstatus()) status);
      )

  (* (value?) *)
  | O.Glob ->
      pr2 "TODO: interpret Glob";
      ()

  (* (file)[fd] *)
  | O.Write ->
      let t = R.cur () in
      let argv = t.R.argv in
      let pc = t.R.pc in
      (match argv with
      | []       -> E.error "> requires file"
      | x::y::xs -> E.error "> requires singleton"
      | [file] ->
          (try 
            let fd_from = 
              Unix.openfile file [Unix.O_CREAT;Unix.O_WRONLY] 0o666 in
            (* should be stdout *)
            let fd_to =
              let i = int_at_address t !pc in
              file_descr_of_int i
            in
            R.push_redir (R.FromTo (fd_from, fd_to));
            incr pc;
            R.pop_list();
          with Unix.Unix_error (err, s1, s2) ->
            prerr_string (spf "%s: " file);
            E.error "can't open"
          )
              
      )

  | O.Popredir ->
      R.pop_redir ()

  (* (name) *)
  | O.Dollar ->
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [varname] -> 
         (* less: deglob varname *)
         (try 
            let value = vlook_varname_or_index varname in
            R.pop_list ();
            let argv = t.R.argv in
            let newargv = 
              (match value with None -> [] | Some xs -> xs) @ argv in
            t.R.argv <- newargv
          with Failure s -> E.error s
         )             
      | _ -> E.error "variable name not singleton!"
      )
  (* (name) *)
  | O.Count ->
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [varname] -> 
          (* less: deglob *)
          let value = vlook_varname_or_index varname in
          let num = 
            match value with
            | None -> 0
            | Some xs -> List.length xs
          in
          R.pop_list ();
          R.push_word (spf "%d" num)
      | _ -> E.error "variable name not singleton!"
      )

  (* (pat, str) *)
  | O.Match ->
      let t = R.cur () in
      let argv = t.R.argv in
      let subject = String.concat " " argv in
      Status.setstatus "no match";
      R.pop_list ();
      let argv = t.R.argv in
      argv |> List.exists (fun w -> 
        if Pattern.match_str subject w
        then begin
          Status.setstatus "";
          true
        end else false
      ) |> ignore;
      R.pop_list ();

  | O.If ->
      let t = R.cur () in
      let pc = t.R.pc in

      Globals.ifnot := true;
      if Status.truestatus()
      then incr pc
      else pc := int_at_address t (!pc);

  | O.Wastrue ->
      Globals.ifnot := false

  | O.IfNot ->
      let t = R.cur () in
      let pc = t.R.pc in
      if !Globals.ifnot
      then incr pc
      else pc := int_at_address t (!pc);

  (* [addr] *)
  | O.Jump ->
      let t = R.cur () in
      let pc = t.R.pc in
      pc := int_at_address t (!pc);

  (* (pat, value){...} *)
  | O.Case ->
      let t = R.cur () in
      let pc = t.R.pc in
      let s = List.hd t.R.argv_stack |> String.concat " " in
      let argv = t.R.argv in
      let match_found = argv |> List.exists (fun w -> Pattern.match_str s w) in
      (if match_found
      then incr pc
      else pc := int_at_address t (!pc)
      );
      R.pop_list ();

  (* (value) *)
  | O.Popm ->
      R.pop_list ()

  (* (name) *)
  | O.DelFn ->
      let t = R.cur () in
      let argv = t.R.argv in
      argv |> List.iter (fun s ->
        let x = Fn.flook s in
        match x with
        | Some _ -> Hashtbl.remove R.fns s
        (* stricter: *)
        | None -> E.error (spf "deleting undefined function %s" s)
      );

  | O.Not ->
      Status.setstatus (if Status.truestatus() then "false" else "");

  | O.True ->
      let t = R.cur () in
      let pc = t.R.pc in
      if Status.truestatus ()
      then incr pc
      else pc := int_at_address t (!pc)

  | O.False ->
      let t = R.cur () in
      let pc = t.R.pc in
      if Status.truestatus ()
      then pc := int_at_address t (!pc)
      else incr pc

  | O.Eflag ->
      if !Globals.eflagok && not (Status.truestatus())
      then Process.exit (Status.getstatus())

  | (O.Concatenate|O.Stringify    |O.Index|
     O.Unlocal|
     O.Fn|
     O.For|
     O.Read|O.Append |O.ReadWrite|O.Close|O.Dup|O.PipeFd|
     O.Subshell|O.Backquote|O.Async
    ) ->
    failwith ("TODO: " ^ Dumper.s_of_opcode (O.F operation))
