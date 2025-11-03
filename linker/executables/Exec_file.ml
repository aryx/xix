(*s: executables/Exec_file.ml *)

(*s: type [[Exec_file.addr]] *)
type addr = int
[@@deriving show]
(*e: type [[Exec_file.addr]] *)

(*s: type [[Exec_file.header_type]] *)
type header_type =
  | A_out (* Plan9 *)
  | Elf (* Linux *)
  (* TODO: | Elf64 | MachO | PE *)
[@@deriving show]
(*e: type [[Exec_file.header_type]] *)

(*s: type [[Exec_file.sections_size]] *)
type sections_size = {
  text_size: int;
  data_size: int;
  bss_size: int;
  (* TODO? symbol_size? more? *)
}
[@@deriving show]
(*e: type [[Exec_file.sections_size]] *)

(*s: function [[Exec_file.show_linker_config]] *)
(* for ocaml-light to work without deriving *)
let show_linker_config _ = "NO DERIVING"
[@@warning "-32"]
(*e: function [[Exec_file.show_linker_config]] *)

(*s: type [[Exec_file.linker_config]] *)
type linker_config = {
  header_type: header_type;
  arch: Arch.t;
  header_size: int;

  init_text: addr;
  init_round: int;
  init_data: addr option;

  (* less: could be (string, addr) Common.either too *)
  entry_point: string;
}
[@@deriving show]
(*e: type [[Exec_file.linker_config]] *)
(*e: executables/Exec_file.ml *)
