(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp

let warn s loc =
  if !Flags.warn
  then 
    if !Flags.warnerror
    then raise (L.Error (spf "Warning: %s" s, loc))
    else 
      let (file, line) = Location_cpp.final_loc_of_loc loc in
      Logs.warn (fun m -> m "%s:%d: %s" file line s)


(* todo: delete outfile *)
let errorexit s =
  Logs.err (fun m -> m "%s" s);
  exit (-1)
