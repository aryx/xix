(*s: mk/Shellenv.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

(*s: type [[Shellenv.t]] *)
type t = (string * string list) list
(*e: type [[Shellenv.t]] *)

(*s: function [[Shellenv.read_environment]] *)
let read_environment (caps : < Cap.env; ..>) =
  CapUnix.environment caps () |> Array.to_list |> List.map (fun s ->
    if s =~ "\\([^=]+\\)=\\(.*\\)"
    then
      let (var, str) = Regexp_.matched2 s in
      var, Regexp_.split "[ \t]+" str
    else failwith (spf "wrong format for environment variable: %s" s)
  )
(*e: function [[Shellenv.read_environment]] *)
(*e: mk/Shellenv.ml *)
