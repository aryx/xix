(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

let warn s loc =
  if !Flags.warn
  then 
    if !Flags.warnerror
    then raise (Location_cpp.Error (spf "Warning: %s" s, loc))
    else 
      let (file, line) = Location_cpp.final_loc_of_loc loc in
      Logs.warn (fun m -> m "%s:%d: %s" !!file line s)

let errorexit s =
  Logs.err (fun m -> m "%s" s);
  raise (Exit.ExitCode 1)
