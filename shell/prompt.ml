open Common

module R = Runtime

(* true so prompt displayed before the first character is read *)
let doprompt = ref true

let prompt = ref "% "

let pprompt () =
  let t = R.cur () in
  if t.R.iflag then begin
    prerr_string !prompt;
    flush stderr;

    (* set promptstr for the next pprompt() *)
    let promptv = (Var.vlook "prompt").Ast.v in

    prompt := 
      (match promptv with
      | Some [x;y] -> y
       (* stricter? display error message if prompt set no 2 elements?*)
      | Some _ | None -> "\t"
      );
  end;
  incr t.R.line;
  doprompt := false

