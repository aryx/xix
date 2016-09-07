open Common

module A = Ast
module O = Opcode

let compile seq =

  let codebuf = ref [| |] in
  let len_codebuf = ref 0 in
  let idx = ref 0 in

  let codebuf_template = Array.create !len_codebuf (O.I 0) in

  let emit x =
    if !idx = !len_codebuf then begin
      len_codebuf := !len_codebuf + 100;
      codebuf := Array.append !codebuf codebuf_template;
    end;
    !codebuf.(!idx) <- x;
    incr idx
  in
  
  let rec outcode_seq seq eflag =
    raise Todo
  in
  outcode_seq seq !Flags.eflag;
  emit (O.F O.Return);
  (* less: O.F O.End *)
  (* less: heredoc, readhere() *)

  Array.sub !codebuf 0 !idx
  |> (fun x -> if !Flags.dump_opcodes then pr2 (Dumper.s_of_codevec x); x)
