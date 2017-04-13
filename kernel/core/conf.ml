open Types

type confmem = {
  base: phys_addr;
  limit: int; (* less: or phys_addr *) 
  (* less: npage: *)
  
}

type t = {
  ncpu: int;
  mem: confmem list;
  nproc: int;

  (* set in Main.confinit *)
  upages: int;
  kpages: int;

  (* upage+kpage *)
  npages: int;
}

(* less: let config = Hashtbl.create 101 *)
