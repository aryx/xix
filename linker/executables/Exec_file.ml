
type addr = int
[@@deriving show]

type header_type =
  | A_out (* Plan9 *)
  | Elf (* Linux *)
  (* TODO: | Elf64 | MachO | PE *)
[@@deriving show]

type sections_size = {
  text_size: int;
  data_size: int;
  bss_size: int;
  (* TODO? symbol_size? more? *)
}
[@@deriving show]

(* for ocaml-light to work without deriving *)
let show_linker_config _ = "NO DERIVING"
[@@warning "-32"]

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
