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
}

(* less: let config = Hashtbl.create 101 *)
