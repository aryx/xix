open Common

let raw = ref false
let index = ref false

let dump_object file =
  let chan = open_in file in
  let input = IO.input_channel chan in
  let unzipped = Unzip.inflate input in
  
  try
    if !raw
    then 
      let str = IO.read_all unzipped in 
      pr2 str
    else begin
      let obj = Objects.read unzipped in
      let v = Dump.vof_obj obj in
      pr (Ocaml.string_of_v v)
    end
  with Unzip.Error _err ->
    failwith "unzip error"

let dump_index file =
  raise Todo

let dump file =
  if !index
  then dump_index file
  else dump_object file

let cmd = { Cmd.
  name = "dump";
  help = "";
  options = [
    "-raw", Arg.Set raw, " ";
    "-index", Arg.Set index, " ";
  ];
  f = (fun args ->
    match args with
    | [file] -> dump file
    | _ -> failwith (spf "dump command [%s] not supported"
                       (String.concat ";" args))
  );
}

