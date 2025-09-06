
type t =
  | PL of pl_type
  | Obj of string
  | Binary of string
  | Text of string
  | Doc of string
  | Config of config_type
  | Media of media_type
  | Archive of string
  | Other of string

and pl_type =
  | OCaml of string
  | FSharp of string
  | MLOther of string
  | Haskell of string
  | Lisp of lisp_type
  | Skip
  | Scala
  | Prolog of string
  | Script of string
  | C of string
  | Cplusplus of string
  | Java
  | Kotlin
  | Csharp
  | ObjectiveC of string
  | Swift
  | Julia
  | Perl
  | Python
  | Ruby
  | Lua
  | R
  | Erlang
  | Go
  | Rust
  | Move
  | Beta
  | Pascal
  | Haxe
  | Bytecode of string
  | Asm
  | Web of webpl_type
  | IDL of idl_type
  | MiscPL of string
  | Elixir

and config_type =
  | Makefile
  | Dockerfile
  | Json
  | Jsonnet
  | Properties
  | Ignore of string
  | RC of string
  | Yaml
  | Terraform
  | Sexp
  | Toml

and lisp_type = CommonLisp | Elisp | Scheme | Clojure

and webpl_type =
  | Php of string
  | Hack
  | Js
  | TypeScript (* JSX/TSX are converted in Js/Typescript *)
  | Coffee
  | Vue
  | Css
  | Scss
  | Html
  | Xml
  | Opa
  | Flash
  | Sql

and idl_type = Thrift | ATD | Protobuf

and media_type = Sound of string | Picture of string | Video of string


(* main entry point (note that this function might read the file
 * or Unix.stat it to get its size so the file must exist!)
 *)
val of_file : Fpath.t -> t

(* is_xxx helpers *)
val is_textual_file : Fpath.t -> bool
val is_json_file : Fpath.t -> bool
val is_syncweb_obj_file : Fpath.t -> bool

(* specialisations *)
val webpl_type_of_file : Fpath.t -> webpl_type option

(*
val files_of_dirs_or_files :
  < Cap.readdir ; .. > -> (t -> bool) -> Fpath.t list -> Fpath.t list
*)
