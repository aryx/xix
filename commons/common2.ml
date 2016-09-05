
let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let enum x n =
  if not(x <= n)
  then failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n::acc else enum_aux (x::acc) (x+1) n
  in
  List.rev (enum_aux [] x n)

let (list_of_string: string -> char list) = function
    "" -> []
  | s -> (enum 0 ((String.length s) - 1) |> List.map (String.get s))



let command2 s = 
  ignore(Sys.command s)

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
  Sys.command ("rm -f " ^ nm) |> ignore;
  String.concat "\n" (List.rev !lines)
