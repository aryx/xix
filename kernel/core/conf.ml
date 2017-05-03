open Types

type confmem = {
  base: phys_addr;
  limit: phys_addr;
  (* less: npage: *)
  
}

type t = {
  ncpu: int;
  mem: confmem list;
  nproc: int;

  (* set in Main.confinit *)
  user_pages: int;
  kernel_pages: int;

  (* upage+kpage *)
  npages: int;
}

(* less: let config = Hashtbl.create 101 *)
