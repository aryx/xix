
(* Run a closure in a separate children process and returns its value.
 *
 * This is especially useful in a testing context such as e2e testing of a CLI
 * function that may modify many global variables that you don't want
 * to reset after the test. Running this function in a children separate
 * process solves the issue.
 *
 * Limitations: 
 *  - the closure may raise exceptions, but you will not
 *    be able to match them in the caller because of Marshal.ml limitations
 *    (see the note about it in CapProcess.ml and marshal.mli).
 *    If you want to, you will need to define a regular type (e.g.,
 *    type exn_res = ExnNot_found of string) and in the closure convert
 *    exns in variants of this regular type).
 *  - the closure may not use [exit] (UnixExit is fine) as this will
 *    raise an End_of_file error.
 * history:
 *  - was called CapProcess.ml in semgrep
 *)
val apply_in_child_process :
  < Cap.fork; .. > -> (*?flags:Marshal.extern_flags list ->*)
  ('a -> 'b) -> 'a -> 'b

(*
 * The unit argument is actually so that a call to
 * [apply_in_child_process_promise caps f args] can return a promise on the
 * result.
 *)
val apply_in_child_process_promise :
  < Cap.fork; .. > ->
  (*?flags:Marshal.extern_flags list -> *)
  ('a -> 'b) ->
  'a ->
  unit ->
  'b

type pid = int
