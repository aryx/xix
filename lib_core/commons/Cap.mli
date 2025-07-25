(* Capabilities implemented as simple abstract types and explicit
 * (object) arguments/parameters ("Lambda the ultimate security tool").
 *
 * Note that most of the types below are on purpose abstract and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below. This function is passing all capabilities
 * to the entry point of your program; this entry point can then restrict
 * the set of capabilities to pass to other functions by using the :> cast
 * operator.
 *)

(**************************************************************************)
(* Standard capabilities *)
(**************************************************************************)

module Console_ : sig
  type stdin
  type stdout
  type stderr
  (* logs are an "ambient" authority though *)
end

module Process : sig
  type argv
  type env
  type exit
  type chdir
  type signal
  type time_limit
  type memory_limit
  type fork
  type wait
end

module FS_ : sig
  type readdir
  type tmp
  (* TODO: refine in open_argv, open_pwd, open_root *)
  type open_in
  type open_out
end

module Exec : sig
  (* note that you can make your own exec capability (e.g., git_exec)
   * a subtype of this one by defining your own function
   * that takes Exec.t and gives the subcapability
   *)
  type t
end

module Network : sig
  type t
end

(* If your program does not use the capabilities below, it has the nice
 * property of being deterministic!
 *)
module Misc : sig
  type random
end

(**************************************************************************)
(* Powerbox *)
(**************************************************************************)

(* fs *)
type readdir = < readdir : FS_.readdir >
type tmp = < tmp : FS_.tmp >
type open_in = < open_in : FS_.open_in >
type open_out = < open_out : FS_.open_out >
type fs = < readdir ; tmp; open_in; open_out >

(* console *)
type stdin = < stdin : Console_.stdin >
type stdout = < stdout : Console_.stdout >
type stderr = < stderr : Console_.stderr >
type console = < stdin ; stdout ; stderr >

(* process *)
type argv = < argv : Process.argv >
type env = < env : Process.env >
type signal = < signal : Process.signal >
type time_limit = < time_limit : Process.time_limit >
type memory_limit = < memory_limit : Process.memory_limit >
type exit = < exit : Process.exit >
type chdir = < chdir : Process.chdir >
type fork = < fork : Process.fork >
type wait = < wait : Process.wait >
type process_multi = < fork; wait >
type process_single = < signal ; time_limit ; memory_limit ; exit ; chdir >
type process = < argv ; env; console ; process_single ; process_multi >

(* exec *)
type exec = < exec : Exec.t >

(* network *)
type network = < network : Network.t >

(* misc *)
type random = < random : Misc.random >
type misc = < random >

(* alt: called "Stdenv.Base.env" in EIO *)
type all_caps =
  < process
  ; fs
  ; exec (* exec is a mix of fs and process_multi as it requires both *)
  ; network
  ; misc >

(* you can also pass individual capabilities like just
 * stdout with 'Console.stdout'
 *)

(* pure computation, just cpu/ram *)
type no_caps = < >

(* In theory, you should not need this constant but when refactoring code
 * it can happen that a function temporarily does not need anymore capabilities
 * but the person knows it might soon in the future; it can be tedious
 * to each time add/remove the caps argument. In that case, it is simpler to
 * pass the no_caps below.
 *)
val no_caps : no_caps

(**************************************************************************)
(* Temporary unsafe caps to help migration *)
(**************************************************************************)
(* !!DO NOT USE!! *)
val network_caps_UNSAFE : unit -> < network >
val tmp_caps_UNSAFE : unit -> < tmp >
val stdout_caps_UNSAFE : unit -> < stdout >
val readdir_UNSAFE : unit -> < readdir >

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

(* Only way to access capabilities. This must be restricted to be called
 * only from a Main.ml (or Test.ml). In any case, it can't be called
 * twice in your program (there's a dynamic check for it).
 *)
val main : (all_caps -> 'a) -> 'a
