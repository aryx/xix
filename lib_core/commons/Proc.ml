(* Copyright 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Process helpers.
 *
 * history:
 *  - harrop article and pfff/commons/parallel.ml
 *  - pfff/commons/distribution.ml
 *  - xix/lib_core/commons/CapProcess.ml
 *
 * alt: we could name this file Process.ml but this conflict with 
 * xix/shell/Process.ml and maybe a 4 letters name is not bad and consistent
 * with Chan.ml, Logs.ml, etc.
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* src: harrop article on fork-based parallelism
 * returns a promise.
 * old: was called invoke() and was in pfff/commons/parallel.ml
 * related work: my pfff/commons/distribution.ml
 *)
let apply_in_child_process_promise (caps : < Cap.fork; .. >) ?(flags = []) f x =
  let input, output = Unix.pipe () in
  match CapUnix.fork caps () with
  (* error, could not create process, well compute now then *)
  | -1 ->
      let v = f x in
      fun () -> v
  (* child *)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output
        (try `Res (f x) with
        | e -> `Exn e)
        flags;
      close_out output;
      (* nosemgrep: do-not-use-exit *)
      exit 0
  (* parent *)
  | pid -> (
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = Marshal.from_channel input in
        (* Without 'WNOHANG', in macOS the 'waitpid' call may fail with 'EINTR',
         * not 100% sure why. *)
        ignore Unix.(waitpid [ WNOHANG ] pid);
        close_in input;
        match v with
        | `Res x -> x
        | `Exn e ->
            (* From marshal.mli in the OCaml stdlib:
             *  "Values of extensible variant types, for example exceptions (of
             *  extensible type [exn]), returned by the unmarshaller should not be
             *  pattern-matched over through [match ... with] or [try ... with],
             *  because unmarshalling does not preserve the information required for
             *  matching their constructors. Structural equalities with other
             *  extensible variant values does not work either.  Most other uses such
             *  as Printexc.to_string, will still work as expected."
             *)
            (* TODO: do not raise; the exn could not be matched in the caller
             * so better to use Printexc.to_string and return that.
             *)
            raise e)

let apply_in_child_process (caps : < Cap.fork; .. >) ?flags f x =
  apply_in_child_process_promise caps ?flags f x ()
