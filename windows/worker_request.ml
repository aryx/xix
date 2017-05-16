
(* was called xfid in rio-C *)
type t = {

  (* todo: 
   * req: fcall.t; 
   *)
  (* answer buffer *)
  ans: string;

  (* handler to worker thread *)
  (* todo: chan: (t -> unit) Event.channel; *)
  
  fid: Fid.t;
  (* less: Ref? Mutex.t? *)
}
