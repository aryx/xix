(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(* Types *)
(**************************************************************************)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)

exception Bad of string
(** Functions in [spec] or [anon_fun] can raise [Arg.Bad] with an error
    message to reject invalid arguments.
    [Arg.Bad] is also raised by {!Arg.parse_argv} in case of an error. *)

exception Help of string
(** Raised by [Arg.parse_argv] when the user asks for help. *)

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

(**************************************************************************)
(* Helpers *)
(**************************************************************************)

exception Stop of error (* used internally *)

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3)::t when y1 = x -> y2
  | _::t -> assoc3 x t


let help_action () = raise (Stop (Unknown "-help"))

let add_help speclist =
  let add1 =
    try ignore (assoc3 "-help" speclist); []
    with Not_found ->
            ["-help", Unit help_action, " Display this list of options"]
  and add2 =
    try ignore (assoc3 "--help" speclist); []
    with Not_found ->
            ["--help", Unit help_action, " Display this list of options"]
  in
  speclist @ (add1 @ add2)

let usage_b buf speclist errmsg =
  bprintf buf "%s\n" errmsg;
  List.iter (function (key, _, doc) -> bprintf buf "  %s %s\n" key doc) 
    (add_help speclist)

let usage_string speclist errmsg =
  let b = Buffer.create 200 in
  usage_b b speclist errmsg;
  Buffer.contents b

let usage speclist errmsg =
  eprintf "%s" (usage_string speclist errmsg)

let current = ref 0

(**************************************************************************)
(* API entry point *)
(**************************************************************************)

let parse_argv argv speclist anonfun errmsg =
  let initpos = !current in
  let convert_error error =
    (* convert an internal error to a Bad/Help exception
       *or* add the program name as a prefix and the usage message as a suffix
       to an user-raised Bad exception.
    *)
    let b = Buffer.create 200 in
    let progname =
      if initpos < (Array.length argv) then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          bprintf b "%s: unknown option '%s'.\n" progname s
      | Missing s ->
          bprintf b "%s: option '%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          bprintf b "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  progname arg opt expected
      | Message s -> (* user error message *)
          bprintf b "%s: %s.\n" progname s
    end;
    usage_b b speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then Help (Buffer.contents b)
    else Bad (Buffer.contents b)
  in
  let l = Array.length argv in
  incr current;
  while !current < l do
   begin try
    let s = argv.(!current) in
    if String.length s >= 1 & String.get s 0 = '-' then begin
      let action =
        try assoc3 s speclist
        with Not_found -> raise (Stop (Unknown s))
      in
      begin
        match action with
        | Unit f -> f ();
        | Bool f ->
            let arg = argv.(!current + 1) in
            begin try f (bool_of_string arg)
            with Invalid_argument "bool_of_string" ->
                   raise (Stop (Wrong (s, arg, "a boolean")))
            end;
            incr current;
        | Set r -> r := true;
        | Clear r -> r := false;
        | String f when !current + 1 < l ->
            let arg = argv.(!current+1) in
            f arg;
            incr current;
        | Set_string r when !current + 1 < l ->
            r := argv.(!current+1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current+1) in
            begin try f (int_of_string arg)
            with Failure "int_of_string" -> raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current+1) in
            begin try r := (int_of_string arg)
            with Failure "int_of_string" -> raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Float f when !current + 1 < l ->
            let arg = argv.(!current+1) in
            f (float_of_string arg);
            incr current;
        | Set_float r when !current + 1 < l ->
            let arg = argv.(!current+1) in
            begin try r := (float_of_string arg);
            with Failure "float_of_string" -> raise (Stop (Wrong (s, arg, "a float")))
            end;
            incr current;
        | _ -> raise (Stop (Missing s))
      end;
    end else begin
      (try anonfun s with Bad m -> raise (convert_error (Message m)));
    end;
   with 
      | Bad m -> raise (convert_error (Message m))
      | Stop e -> raise (convert_error e)
   end;
   incr current;
  done;
  ()


let parse l f msg =
  try
    parse_argv Sys.argv l f msg
  with
  | Bad msg -> eprintf "%s" msg; exit 2
  | Help msg -> printf "%s" msg; exit 0


(**************************************************************************)
(* Align *)
(**************************************************************************)

let second_word s =
  let len = String.length s in
  let rec loop n =
    if n >= len then len
    else if s.[n] = ' ' then loop (n+1)
    else n
  in
  try
    let n = String.index s '\t' in
    loop (n+1)
  with Not_found ->
      begin 
        try 
          let n = String.index s ' ' in
          loop (n+1)
        with Not_found -> len
      end

let replace_leading_tab s =
  let seen = ref false in
  String.map (function '\t' when not !seen -> seen := true; ' ' | c -> c) s

let add_padding len ksd =
  match ksd with
  | (_, _, "") ->
      (* Do not pad undocumented options, so that they still don't show up when
       * run through [usage] or [parse]. *)
      ksd
(*
  | (kwd, (Symbol _ as spec), msg) ->
      let cutcol = second_word msg in
      let spaces = String.make ((Int.max 0 (len - cutcol)) + 3) ' ' in
      (kwd, spec, "\n" ^ spaces ^ replace_leading_tab msg)
*)
  | (kwd, spec, msg) ->
      let cutcol = second_word msg in
      let kwd_len = String.length kwd in
      let diff = len - kwd_len - cutcol in
      if diff <= 0 then
        (kwd, spec, replace_leading_tab msg)
      else
        let spaces = String.make diff ' ' in
        let prefix = String.sub (replace_leading_tab msg) 0 cutcol in
        let suffix = String.sub msg cutcol (String.length msg - cutcol) in
        (kwd, spec, prefix ^ spaces ^ suffix)


let max_arg_len cur (kwd, spec, doc) =
  match spec with
  (* | Symbol _ -> Int.max cur (String.length kwd) *)
  | _ -> (*Int.*)max cur (String.length kwd + second_word doc)

let align (* ?(limit=max_int)*) speclist =
  let limit = max_int in
  let completed = add_help speclist in
  let len = List.fold_left max_arg_len 0 completed in
  let len = (*Int.*)min len limit in
  List.map (add_padding len) completed
