open Common
open Types

type t_s = Types.t_s
type t_ms = Types.t_ms
type t_us = Types.t_us
type t_ns = Types.t_ns

(* incremented Arch.hz times per second in hz_clock () (kernel clock) *)
type t_ticks = Types.t_ticks
(* incremented at system timer frequency (system clock) *)
type t_fastticks = Types.t_fastticks

let tick_to_ms tk =
  tk * 1000 / Arch.hz
  
