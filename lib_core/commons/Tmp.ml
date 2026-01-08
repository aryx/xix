open Fpath_.Operators

let new_file (caps : < Cap.tmp; ..>) prefix suffix =
  let _ = caps#tmp in
  (* nosemgrep: use-caps *)
  let filename = Filename.temp_file prefix ("." ^ suffix) in
  Logs.info (fun m -> m "creating %s" filename);
  Fpath.v filename

let with_new_file (caps : < Cap.tmp; ..>) prefix suffix f =
  let file = new_file caps prefix suffix in
  Fun.protect ~finally:(fun () ->
      (* nosemgrep: use-caps *)
      Sys.remove !!file
  ) (fun () -> f file)
