
type pid = int

type state = 
  | Dead
  | Running

  | Ready
  | Scheding

  | Wakeme

  | Broken
  | Moribund
  | Stopped

  | Rendezvous
  | Waitrelease

  (* less: factorize of R | W option ?*)
  | Queueing
  | QueueingR
  | QueueingW

type t = {
  pid: pid;
  state: state;

  slash: Chan.t ref;
  dot: Chan.t ref;
}
