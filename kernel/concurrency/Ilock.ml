open Common

open Types
open Ilock_

type t = Ilock_.t

let lock (x : t) : unit =
  let when_hold oldprio =
    let _up = Globals.up () in
    (* less: 
     * - update up.last_ilock
     * - increment ilockdepths
     * No need to increment up.nlocks. We have disabled interrupts 
     * so no risk of getting scheduled out.
     * No need to save current process doing the lock? because
     * often used in interrupt context?
     * 
    *)
    x.saved_spl <- oldprio;
    (* less: add more debugging info in lock once you grab it *)
    ()
  in
  let oldprio = Spl.high () in
  if Tas.tas x.hold = false
  then when_hold oldprio (* good to go! *)
  else begin
    (* we have to spin *)
    let finish = ref false in
    while not !finish do
      Spl.set oldprio;
      (* less: coherence issue? *)
      while not !(x.hold) do
        ()
      done;
      (* let's try again *)
      let oldprio = Spl.high () in
      if Tas.tas x.hold = false
      then begin 
        when_hold oldprio;
        finish := true;
      end
    done
  end

let unlock (x : t) : unit =
  if not !(x.hold)
  then failwith "Spinlock.unlock: not locked";
  (* less: if Spl.is_low failwith? *)
  let oldprio = x.saved_spl in
  (* less: coherence issue? *)
  x.hold := false;
  (* less: ilockdepth decrement, update last_ilock *)
  Spl.set oldprio
    

let alloc () : t =
  { hold = ref false;
    saved_spl = Spl_.Low;
  }
