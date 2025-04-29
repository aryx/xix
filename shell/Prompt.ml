open Common

module R = Runtime

(* Set to true so the prompt is displayed before the first character is read.
 *
 * Do we need that global? Can we not just call pprompt() explicitely?
 * No because when we get a newline, we know we should display the prompt,
 * but not before finishing executing the command. So the display
 * prompt should be done before the next round of input.
 * less: actually we could do it in the caller of parse_line, in the REPL.
 *)
let doprompt = ref true

let prompt = ref "% "

let pprompt () =
  let t = R.cur () in
  if t.R.iflag then begin
    prerr_string !prompt;
    flush stderr;

    (* set promptstr for the next pprompt() *)
    let promptv = (Var.vlook "prompt").R.v in
    prompt := 
      (match promptv with
      | Some [x;y] -> y
       (* stricter? display error message if prompt set no 2 elements?*)
      | Some _ | None -> "\t"
      );
  end;
  (* alt: incr t.R.line; this is done in the lexer instead *)
  doprompt := false
