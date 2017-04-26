open Common

let usage = 
  spf "usage: [-options] -addr 0x80001000 syms.list"

let parse file =
  let xs = Common.cat file in
  let procs = 
    xs |> Common.map_filter (fun s ->
      let xs = Str.split (Str.regexp "[ \t]+") s in
      match xs with
      | [("T" | "t"); addr; sym] ->
        let i = int_of_string ("0x" ^ addr) in
        Some (sym, i)
      | _ -> None
    )
  in
  Common.sort_by_val_lowfirst procs

let debug addr file =
  let procs = parse file in
  let rec aux xs =
    match xs with
    | (s1, addr1)::(s2, addr2)::xs ->
      if addr >= addr1 && addr < addr2
      then pr (spf "proc = %s at %x" s1 addr1)
      else aux ((s2, addr2)::xs)
    | [] | [_] -> failwith "procedure not found"
  in
  aux procs

let main () =
  let infile  = ref "" in
  let addr = ref 0 in

  let options = [
    "-addr", Arg.Set_int addr,
    " <int> address to print debug information on";
  ]
  in
  Arg.parse options
   (fun f -> 
     if !infile <> ""
     then failwith "already specified an input file";
     infile := f;
   )
   usage;

  if !infile = ""
  then begin Arg.usage options usage; exit (-1); end;

  debug !addr !infile 

let _ = 
  main ()
