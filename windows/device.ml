open Common

type t = {
  open_: File.t -> unit;
  close: File.t -> unit;
  read_threaded: int64 -> int -> File.t -> bytes;
}
