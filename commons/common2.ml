
let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let command2 s = ignore(Sys.command s)


(* julia: convert something printed using format to print into a string *)
let format_to_string f =
  let (nm,o) = Filename.open_temp_file "format_to_s" ".out" in
  (* to avoid interference with other code using Format.printf, e.g.
   * Ounit.run_tt
   *)
  Format.print_flush();
  Format.set_formatter_out_channel o;
  let _ = f () in
  Format.print_newline();
  Format.print_flush();
  Format.set_formatter_out_channel stdout;
  close_out o;
  let i = open_in nm in
  let lines = ref [] in
  let rec loop _ =
    let cur = input_line i in
    lines := cur :: !lines;
    loop() in
  (try loop() with End_of_file -> ());
  close_in i;
  command2 ("rm -f " ^ nm);
  String.concat "\n" (List.rev !lines)
