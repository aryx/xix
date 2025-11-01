
type t_s = Types.t_s
type t_ms = Types.t_ms
type t_us = Types.t_us
type t_ns = Types.t_ns

(* incremented Arch.hz times per second in hz_clock () (kernel clock) *)
type t_ticks = Types.t_ticks
(* incremented at system timer frequency (system clock) *)
type t_fastticks = Types.t_fastticks (* itself an alias for Arch.t_fastticks *)

val tick_to_ms : t_ticks -> t_ms

val ms_to_tick : t_ms -> t_ticks
val ns_to_fastticks : t_ns -> t_fastticks
val ms_to_ns : t_ms -> t_ns
