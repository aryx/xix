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
      pr2 (spf "%s:%d warning: %s" file line s)


(* todo: delete outfile *)
let errorexit s =
  pr2 s;
  exit (-1)
