
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
