open Common

module O = Opcode
module R = Runtime
module E = Error

open Opcode (* just for big dispatch error case below *)

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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let interpret operation =
  match operation with
  | O.REPL -> Op_repl.op_REPL ()

  | O.Simple -> Op_process.op_Simple ()

  | O.Return -> Process.return ()
  | O.Exit -> 
      (* todo: trapreq *)
      Process.exit (Status.getstatus())

  | O.Mark -> R.push_list ()
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
  (* Assign (varname) (args) *)
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
        newt.R.redirections <- 
          (R.FromTo (pipe_write, lfd))::newt.R.redirections;
      (* parent *)
      end else begin
        (* less: addwaitpid () *)
        let newt = 
          R.mk_thread t.R.code (int_at_address t (!pc+0)) t.R.locals in
        R.runq := newt::!R.runq;
        Unix.close pipe_write;
        newt.R.redirections <- 
          (R.FromTo (pipe_read, rfd))::newt.R.redirections;
      
       (* once newt finished, jump to Xpipewait *)
        pc := int_at_address t (!pc+1);
        t.R.waitstatus <- R.WaitFor forkid;
      end


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


  | (Popm|
     Count|Concatenate|Stringify|Glob|Dollar|Index|
     Local|Unlocal|Fn|DelFn|
     If|IfNot|Jump|Match|Case|For|Wastrue|Bang|False|True|
     Read|Write|ReadWrite|Append|Close|Dup|PipeFd|
     Error|Eflag|
     Subshell|Backquote|Async
    ) ->
    failwith ("TODO: " ^ Dumper.s_of_opcode (O.F operation))
