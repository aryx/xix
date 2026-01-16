(*s: shell/Env.ml *)
open Common
open Regexp.Operators

type t = (string * string list) list

(* copy paste of mk/Shellenv.ml 
 * alt: move to lib_core/commons/Proc.ml to factorize?
 * This is Unix specific; In plan9 one needs to read /env/.
*)
let read_environment (caps : < Cap.env; ..>) =
  CapUnix.environment caps () |> Array.to_list |> List.map (fun s ->
    if s =~ "\\([^=]+\\)=\\(.*\\)"
    then
      let (var, str) = Regexp.matched2 s in
      var, Regexp.split "[ \t]+" str
    else failwith (spf "wrong format for environment variable: %s" s)
  )

(*e: shell/Env.ml *)
