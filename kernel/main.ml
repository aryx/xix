
let main =
  (* this works because of some special code in Byterun/io.c 
   * to redirect printing to screenputs when printing on descriptor 1 or 2
   *)
  Common2._print := print_string;

  Test.test ()
(*
  let call = Syscall.Nop in
  Syscall_dispatch.dispatch call
*)
    
(* in C: mmuinit1; confinit (); xinit();  screeninit (); trapinit();
 * todo:
 * Page.init_allocator ()
 * Proc.init_allocator ()
 * 
 * arch_userinit()
 * Scheduler.init()
*)
