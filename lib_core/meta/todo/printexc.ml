(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Printf;;

let locfmt =
  match Sys.os_type with
  | _ -> ("File \"%s\", line %d, characters %d-%d: %s" : ('a, 'b, 'c) format)
;;

let field x i =
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = 252 then
    sprintf "\"%s\"" (String.escaped (Obj.magic f : string))
  else if Obj.tag f = 253 then
    string_of_float (Obj.magic f : float)
  else
    "_"
;;
let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))
;;
let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | n -> sprintf "(%s%s)" (field x 1) (other_fields x 2)
;;

let to_string = function
  | Out_of_memory -> "Out of memory";
  | Stack_overflow -> "Stack overflow";
  | Match_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Pattern matching failed";
  | Assert_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Assertion failed";
  | x ->
      let x = Obj.repr x in
      let constructor = (Obj.magic(Obj.field (Obj.field x 0) 0) : string) in
      constructor ^ (fields x)
;;

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2


type loc_info =
  | Known_location of bool   (* is_raise *)
                    * string (* module *)
                    * int    (* pos *)
  | Unknown_location of bool (*is_raise*)

external get_exception_backtrace: 
  unit -> loc_info array option = "caml_get_exception_backtrace"


let format_loc_info pos li =
  let is_raise =
    match li with
    | Known_location(is_raise, _, _) -> is_raise
    | Unknown_location(is_raise) -> is_raise in
  let info =
    if is_raise 
    then
      if pos = 0 then "Raised at" else "Re-raised at"
    else
      if pos = 0 then "Raised by primitive operation at" else "Called from"
  in
  match li with
  | Known_location(is_raise, modname, charpos) ->
      sprintf "%s module %s, character %d"
              info modname charpos
  | Unknown_location(is_raise) ->
      sprintf "%s unknown location"
              info

let get_backtrace () =
  match get_exception_backtrace() with
  | None ->
     "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
      let b = Buffer.create 1024 in
      for i = 0 to Array.length a - 1 do
        if a.(i) <> Unknown_location true 
        then Buffer.add_string b 
               (Printf.sprintf "%s\n" (format_loc_info i a.(i)))
      done;
      Buffer.contents b
