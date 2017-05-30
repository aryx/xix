open Common

let main () =
  let res = Plan9.bind "/tests/xxx" "/" Plan9.mAFTER in
  let file = Unix.openfile "/test_plan9" [Unix.O_RDWR] 0o666 in
  Unix.close file


let _ =
  main ()
