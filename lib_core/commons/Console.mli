(*
   Utilities to help printing user-facing messages (with optional color) on
   stdout and stderr.

   Programs such as Semgrep use both stdout and stderr to display
   human-readable messages.

   See UConsole.ml or better CapConsole.ml to actually print messages.
   This is the shared "safe" part of console management.
*)

(* The result of applying 'highlight_setting' *)
type highlight = On | Off [@@deriving show]

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

(* internals, you should not change that, only UConsole.setup can *)
val highlight : highlight ref
