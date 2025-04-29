(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

type t = (string * string list) list

let read_environment () =
  Unix.environment () |> Array.to_list |> List.map (fun s ->
    if s =~ "\\([^=]+\\)=\\(.*\\)"
    then
      let (var, str) = Regexp_.matched2 s in
      var, Regexp_.split "[ \t]+" str
    else failwith (spf "wrong format for environment variable: %s" s)
  )
