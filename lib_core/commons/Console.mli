(*
   Utilities to help printing user-facing messages (with optional color) on
   stdout and stderr.

   Programs such as Semgrep use both stdout and stderr to display
   human-readable messages.
*)

(* Print a string, print a newline, and flush the stdout channel. *)
val print : <Cap.stdout; .. > -> string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : <Cap.stdout; .. > -> string -> unit

(* Print a string, print a newline, and flush the stderr channel.
 * You should avoid using this function; Prefer Logs.err or Logs.warn
 * in general.
 *)
val eprint : < Cap.stderr; .. > -> string -> unit

val stdin: <Cap.stdin; .. > -> in_channel
val stdout: <Cap.stdout; .. > -> out_channel
val stderr: <Cap.stderr; .. > -> out_channel

type highlight = On | Off [@@deriving show]

(* internals, you should not change that, only UConsole.setup can *)
val highlight : highlight ref

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default  (** Default color of the terminal *)

type style =
  | Reset
  | Bold
  | Underlined
  | Blink
  | Inverse
  | Hidden
  | Foreground of color
  | Background of color

(* Shortcuts for [Foreground xxx] *)
val black : style
val red : style
val green : style
val yellow : style
val blue : style
val magenta : style
val cyan : style
val white : style
val default : style

(* Will display special color/style ANSI sequence if highlighting is on.
 * Note that this will not work on the default Windows terminal
 * (install a fancier ANSI compatible one or use VSCode which integrates one)
 *)
val sprintf : style -> ('a, unit, string) format -> 'a

(* sprintf shortcuts *)
val color : style -> string -> string
val bold : string -> string
val underline : string -> string

(*
   These functions turn a string into color (red, yellow, or green)
   if highlighting is on.
*)
val error : string -> string
val warning : string -> string
val success : string -> string
