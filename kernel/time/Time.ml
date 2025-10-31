open Common
open Types

type t_s = Types.t_s
type t_ms = Types.t_ms
type t_us = Types.t_us
type t_ns = Types.t_ns

(* incremented Arch.hz times per second in hz_clock () (kernel clock) *)
type t_ticks = Types.t_ticks
(* incremented at system timer frequency (system clock) *)
type t_fastticks = Types.t_fastticks (* itself an alias for Arch.t_fastticks *)

let mhz = 1000 * 1000


(* a tick is 10ms when Arch.hz is 100 *)
let tick_to_ms tk =
  tk * 1000 / Arch.hz

(* + 500 to average up. 5ms is counted as 1 tick when Arch.hz is 100 *)
let ms_to_tick ms =
  (ms * Arch.hz + 500) / 1000

let ns_to_fastticks _ns =
  raise Todo

let ms_to_ns ms =
  ms * 100000
