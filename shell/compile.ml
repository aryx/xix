open Common

module A = Ast
module O = Opcode

(* todo: need pass eflag in all the subfunc below? *)
let outcode_seq seq eflag emit idx =

  let rec xseq seq eflag =
   (* less set iflast *)
    seq |> List.iter (fun x -> xcmd x eflag)
  
  and xcmd cmd eflag =
    match cmd with
    | A.Simple (w, ws) -> 
        emit (O.F O.Mark);
        xwords ws eflag;
        xword w eflag;
        emit (O.F O.Simple);
        if eflag then emit (O.F O.Eflag);
        
    | _ -> failwith ("TODO: " ^ Dumper.s_of_cmd cmd)
  and xword w eflag =
    match w with
    | A.Word (s, _quoted) ->
        emit (O.F O.Word);
        emit (O.S s);

    | _ -> failwith ("TODO: " ^ Dumper.s_of_value w)
  and xwords ws eflag =
    ws |> List.rev |> List.iter (fun w -> xword w eflag);
    
  in
  xseq seq eflag


let compile seq =

  let codebuf = ref [| |] in
  let len_codebuf = ref 0 in
  let idx = ref 0 in

  let codebuf_template = Array.create 100 (O.I 0) in

  let emit x =
    if !idx = !len_codebuf then begin
      len_codebuf := !len_codebuf + 100;
      codebuf := Array.append !codebuf codebuf_template;
    end;
    !codebuf.(!idx) <- x;
    incr idx
  in
  
  outcode_seq seq !Flags.eflag emit idx;
  emit (O.F O.Return);
  (* less: O.F O.End *)
  (* less: heredoc, readhere() *)

  Array.sub !codebuf 0 !idx
  |> (fun x -> if !Flags.dump_opcodes then pr2 (Dumper.s_of_codevec x); x)
