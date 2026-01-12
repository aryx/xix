(* Capabilities implemented as simple abstract types and explicit
 * (object) arguments/parameters ("Lambda the ultimate security tool").
 *
 * Here is an example of use:
 *
 *   let foo (caps : < Cap.stdout; ..>) str =
 *      CapConsole.print caps str
 *
 *   let bar (caps : < Cap.stdout; Cap.stderr; ..) str = 
 *      CapConsole.eprint caps str;
 *      foo str
 *   
 *   let _main =
 *     Cap.main (fun all_caps -> bar all_caps)
 *
 * Thx to capabilities, you can see in the signature of functions the
 * set of capabilities that are needed, a bit like an effect system.
 *
 * Note that you must also find a way to forbid your code to call
 * the dangerous standard lib functions that are not capability-aware
 * (e.g., Stdlib.printf, Unix.stdout, etc.). One way is to use a tool
 * like Semgrep (see xix/semgrep.jsonnet file for an example).
 *
 * For more information, see xix/utilities/text/ed/ for a simple example of
 * use of capabilities in a real program.
 *
 *
 * Note that most of the types below are on purpose abstract and there is
 * no way to build/forge them except by calling the restricted (statically
 * and dynamically) Cap.main() below. This function is passing all capabilities
 * to the entry point of your program; this entry point can then restrict
 * statically the set of capabilities to pass to other functions by using
 * the :> cast operator.
 *
 * Note that you can further restrict dynamically capabilities by
 * overriding the passed object. For example, here is some code from
 * oed to further restrict exec/open:
 *
 *    type caps = < Cap.exec; Cap.open_in; Cap.open_out; Cap.fork; Cap.stdin>
 *    let restrict_caps rflag (x : < caps; ..>) =
 *      object
 *        method exec _cmd = 
 *          if rflag then failwith "!restricted mode on!"
 *          else x#exec cmd
 *        method open_in file = 
 *          if rflag && not (Fpath.is_seg file)
 *          then failwith (spf "!restricted mode on, can't read %s!" file)
 *          else x#open_in file
 *        method open_out file = 
 *          if rflag && not (Fpath.is_seg file) &&
 *             (* need open_out to delete tmp file in Commands.quit() *)
 *             not (file = !!Env.tfname)
 *          then failwith (spf "!restricted mode on, can't write %s!" file)
 *          else x#open_out file
 *        method fork = x#fork
 *        ...
 *      end
 *)

(**************************************************************************)
(* Standard capabilities *)
(**************************************************************************)

module Console_ : sig
  type stdin
  type stdout
  type stderr
  (* logs are an "ambient" authority though *)

  (* plan9 caps *)
  type draw
  type keyboard
  type mouse
end

module Process : sig
  type argv
  type env

  type exit
  type chdir
  type signal

  type time_limit
  type memory_limit

  (* exec has its separate module below *)
  type fork
  type wait
  type kill

  (* plan9 fileservers and namespaces *)
  type mount
  type bind
end

module FS_ : sig
  type readdir
  type tmp
  (* we could refine in open_argv, open_pwd, open_root but you can also implement
   * those restrictions dynamically like in restrict_caps() above *)
  type open_in
  type open_out
end

module Exec : sig
  (* note that you can make your own exec capability (e.g., git_exec)
   * a subtype of this one by defining your own function
   * that takes Exec.t and gives the subcapability.
   * You can also restrict the cap dynamically like in restrict_caps() above.
   *)
  type t
end

module Network : sig
  (* You can also restrict the cap dynamically like in restrict_caps() above
   * for example to whitelist a set of IPs.
   *)
  type t
end

(* If your program does not use the capabilities below, it has the nice
 * property of being deterministic!
 *)
module Misc : sig
  type random
  (* TODO: time *)
end

(**************************************************************************)
(* Shortcuts *)
(**************************************************************************)

(* console *)
type stdin = < stdin : Console_.stdin >
type stdout = < stdout : Console_.stdout >
type stderr = < stderr : Console_.stderr >
type draw = < draw : Console_.draw >
type keyboard = < keyboard : Console_.keyboard >
type mouse = < mouse : Console_.mouse >
type console = < stdin ; stdout ; stderr; draw; keyboard; mouse >

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
type kill = < kill : Process.kill >
type mount = < mount : Process.mount >
type bind = < bind: Process.bind >
type process_multi = < fork; wait; kill >
type process_single = < signal ; time_limit ; memory_limit ; exit ; chdir; mount; bind >
type process = < argv ; env; process_single ; process_multi >

(* fs *)
(* note that open_in/open_out take a file path (as a string) as parameter so
 * one can do extra dynamic check before granting the cap by overwriting
 * the open_in/open_out method from the passed all_caps like in
 * restrict_caps() above.
 *)
type open_in = < open_in : string (* path *) -> FS_.open_in >
type open_out = < open_out : string (* path *) -> FS_.open_out >
type readdir = < readdir : string (* path *) -> FS_.readdir >
type tmp = < tmp : FS_.tmp >
type fs = < readdir ; tmp; open_in; open_out >

(* exec *)
type exec = < exec : string (* cmd *) -> Exec.t >
(* shortcut *)
type forkew = < fork; exec; wait >

(* network *)
type network = < network : string (* IP or name *) -> Network.t >

(* misc *)
type random = < random : Misc.random >
type misc = < random >

(**************************************************************************)
(* Powerbox *)
(**************************************************************************)

type all_caps =
  < console
  ; process
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
val tmp_caps_UNSAFE : unit -> < tmp >

(**************************************************************************)
(* Entry point *)
(**************************************************************************)

(* Only way to access capabilities. This must be restricted to be called
 * only from a Main.ml (or Test.ml). In any case, it can't be called
 * twice in your program (there's a dynamic check for it).
 *)
val main : (all_caps -> 'a) -> 'a
