open Common
open Types
open Proc_

let syscall_await () =
  let up = Globals.up () in

  (* sanity check *)  
  up.childlock |> Spinlock.with_lock (fun () ->
    (* important to check waitq too! child might have exited before 
     * the parent await *)
    if up.nchild = 0 && List.length up.waitq = 0
    then Error.error Error.Enochild;
  );
  
  !Hooks.Scheduler.sleep (* todo: up.wait_rendezvous *)  (fun () -> 
    (* todo: no need lock childlock ?? *)
    up.waitq <> []
  );
  (* ok, got something *)
  Spinlock.lock up.childlock;
  let wmsg = 
    match up.waitq with
    | [] -> raise (Impossible "waitq empty but was sleeping on non empty")
    | x::xs -> 
      up.waitq <- xs;
      x
  in
  Spinlock.unlock up.childlock;
  wmsg
