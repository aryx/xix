(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: thread.ml,v 1.15 1997/04/11 13:57:33 xleroy Exp $ *)

(* User-level threads *)

type t

let critical_section = ref false

type resumption_status =
    Resumed_wakeup
  | Resumed_delay
  | Resumed_join
  | Resumed_select of
      Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
  | Resumed_wait of int * Unix.process_status

(* It is mucho important that the primitives that reschedule are called 
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. Furthermore, the RETURNs must all have the same
   frame size, which means that both the primitives and their ML wrappers
   must take exactly one argument. *)

external thread_initialize : unit -> unit = "thread_initialize"
external thread_new : (unit -> unit) -> t = "thread_new"
external thread_yield : unit -> unit = "thread_yield"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_select :
  Unix.file_descr list * Unix.file_descr list *          (* remember: 1 arg *)
  Unix.file_descr list * float -> resumption_status
  = "thread_select"
external thread_join : t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_delay"
external thread_wait_pid : int -> resumption_status = "thread_wait_pid"
external thread_wakeup : t -> unit = "thread_wakeup"
external thread_self : unit -> t = "thread_self"
external thread_kill : t -> unit = "thread_kill"

external id : t -> int = "thread_id"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let sleep () = critical_section := false; thread_sleep()
let delay duration = thread_delay duration
let join th = thread_join th
let wakeup pid = thread_wakeup pid
let self () = thread_self()
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

let select_aux arg = thread_select arg

let select readfds writefds exceptfds delay =
  match select_aux (readfds, writefds, exceptfds, delay) with
    Resumed_select(r, w, e) -> (r, w, e)
  | _ -> ([], [], [])

let wait_read fd = select_aux([fd], [], [], -1.0); ()
let wait_write fd = select_aux([], [fd], [], -1.0); ()
  
let wait_timed_read fd delay =
  match select_aux([fd], [], [], delay) with
    Resumed_select(_, _, _) -> true
  | _ -> false
let wait_timed_write fd delay =
  match select_aux([], [fd], [], delay) with
    Resumed_select(_, _, _) -> true
  | _ -> false

let wait_pid_aux pid = thread_wait_pid pid

let wait_pid pid = 
  match wait_pid_aux pid with
    Resumed_wait(pid, status) -> (pid, status)
  | _ -> invalid_arg "Thread.wait_pid"

(* For Thread.create, make sure the function passed to thread_new
   always terminates by calling Thread.exit. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        Printexc.print fn arg; exit()
      with x ->
        flush stdout; flush stderr; exit())

(* Preemption *)

let preempt signal =
  if !critical_section then () else thread_yield()

(* Initialization of the scheduler *)

let _ =
(* TODO put back!!  Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt); *)
  thread_initialize()
