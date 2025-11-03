(*s: version_control/compression.mli *)

(*s: signature [[Compression.decompress]] *)
val decompress: 
  IO.input -> IO.input
(*e: signature [[Compression.decompress]] *)
(*s: signature [[Compression.compress]] *)
val compress: 
  IO.input -> 'a IO.output -> unit
(*e: signature [[Compression.compress]] *)
(*e: version_control/compression.mli *)
