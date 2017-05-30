
(* todo? copy https://github.com/mirage/ocaml-9p? but seems heavy
 * on the use of external libraries and modules ...
 *)

(* todo: use Int32.t *)
type int32 = int

module Request = struct
  type t = 
end

module Response = struct
  type t = 
end

type message =
  | Request of Request.t
  | Response of Response.t

(* old: was called Fcall in libcore-C *)
type t = {

  fid: int32;
  tag: int;

  msg: message;
  
}
