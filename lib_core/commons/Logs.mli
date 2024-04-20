(* Poor's man logging library following the interface defined in
 * https://github.com/dbuenzli/logs.
 *)

(* Here are the usage conventions for the Logs library "level"
   from https://erratique.ch/software/logs/doc/Logs/index.html#usage
   (slightly augmented).

   - App: unlike the other levels, this prints ordinary messages without any
     special formatting.
     This is usually desactivated with a --quiet flag.

   - Error ('err'): error condition that prevent the program from running
     normally.

   - Warning ('warn'): suspicious condition that does not prevent the
     program from running normally but may eventually lead to an error
     condition.

   - Info: condition that allows the program *user* to get a better
     understanding of what the program is doing.
     Log messages at this level and above may not clutter up the log
     output nor should they reduce performance significantly. If that's
     the case, log at the Debug level.
     This is usually activated with a --verbose flag.

   - Debug: condition that allows the program *developer* to get a
     better understanding of what the program is doing.
     It may reduce the performance of the application or result in
     unreadable logs unless they're filtered.
     This is usually activated with a --debug flag.

   Attention: Any log message that can't be understood without context
   should be moved to the Debug level!
 *)
type level = App | Error | Warning | Info | Debug

val set_level : level option -> unit

type 'a log = ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

val app : 'a log
val err : 'a log
val warn : 'a log
val info : 'a log
val debug : 'a log

(* Note that src, tags, and reporter are not defined here *)
