(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Eq.Operators
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of wc of Plan 9.
 *
 * Limitations compared to Plan9 version:
 *  - no unicode (runes) support, no -r, no -b
 *
 * Improvements over Plan 9 C version:
 *  - no need sysfatal() calls and error management as default Unix exn
 *    (e.g., file not found) and its pretty printing should be good enough
 *  - no need open/close, use higher-order FS.with_open_in
 *  - less globals, pass conf and stats explicitely (use mutable internally,
 *    but still provide a more functional interface than in C)
 *
 * Still the OCaml version is longer than the C one.
 *
 * See also ocaml-light/examples/basics/wc_unix.ml by Pierre Weis.
*)

type caps = < Cap.open_in; Cap.stdout >

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type conf = {
  mutable show_lines: bool;
  mutable show_chars: bool;
  mutable show_words: bool;
}
let empty_conf () = 
  { show_lines = false; show_chars = false; show_words = false }

type stats = {
  mutable lines: int;
  mutable chars: int;
  mutable words: int;
}
let empty_stats () =
  { lines = 0; chars = 0; words = 0 }

type state = InWord | AfterSpace

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let report (caps: <Cap.stdout; ..>) (conf : conf) (x : stats) (str_opt : string option) =
  let str =
    (if conf.show_lines then spf " %7d" x.lines else "") ^
    (if conf.show_words then spf " %7d" x.words else "") ^
    (if conf.show_chars then spf " %7d" x.chars else "") ^
    (match str_opt with Some s -> spf " %s" s | None -> "")
  in
  Console.print caps str

let sum_stats (xs : stats list) : stats =
  (* alt: use List.iter as we modify in place acc *)
  xs |> List.fold_left (fun acc e ->
     acc.lines <- acc.lines + e.lines;
     acc.chars <- acc.chars + e.chars;
     acc.words <- acc.words + e.words;
     acc
  ) (empty_stats ())

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let wc (_caps : < Cap.stdout; .. >) (chan : Chan.i) : stats =
  Logs.info (fun m -> m "processing %s" (Chan.origin chan));

  let stats = empty_stats () in
  let rec aux state =
    let c = input_char chan.ic in
    stats.chars <- stats.chars + 1;
    match c with
    | '\n' ->
        stats.lines <- stats.lines + 1;
        aux AfterSpace
    | ' ' | '\t' | '\r' ->
        aux AfterSpace
    | _ ->
        (match state with
        | AfterSpace -> 
              stats.words <- stats.words + 1;
              aux InWord
        | InWord ->
              aux InWord
        )
  in
  try 
    aux AfterSpace
  with End_of_file -> stats

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)


let main (caps : <caps; ..>) (argv : string array) : Exit.t =

  let conf : conf = empty_conf () in
  let args = ref [] in
  let options = [
     "-l", Arg.Unit (fun () -> conf.show_lines <- true),
      " show lines count";
     "-c", Arg.Unit (fun () -> conf.show_chars <- true),
      " show characters count";
     "-w", Arg.Unit (fun () -> conf.show_words <- true),
      " show words count";
      (* LATER: -r for runes, -b for ?? *)
  ] |> Arg.align
  in

  let conf =
    if conf =*= empty_conf ()
    then { show_lines = true; show_chars = true; show_words = true }
    else conf
  in
  (try 
    Arg.parse_argv argv options (fun t -> args := t::!args) 
      (spf "usage: %s [-lwc] [file ...]" argv.(0));
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  (* alt: use Arg and process -debug, -verbose, etc. *)
  Logs_.setup (Some Logs.Warning) ();

  (match Array.to_list argv with
  | [] -> raise (Impossible "all programs have at least an argv0")
  | [_argv0] ->
      let chan = Chan.{ ic = stdin; origin = Chan.Stdin } in
      let stats = wc caps chan in
      report caps conf stats None
  | _argv0::xs ->
      let files = Fpath_.of_strings xs in
      let statxs = 
          files |> List.map (fun file ->
              file, FS.with_open_in caps (wc caps) file
          ) in
      statxs |> List.iter (fun (file, stats) -> 
            report caps conf stats (Some !!file));
      if List.length xs > 1
        then
        let total_stats = statxs |> List.map snd |> sum_stats in
        report caps conf total_stats (Some "total")
  );
  Exit.OK

let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps (Exit.catch (fun () -> main caps argv))
  )
