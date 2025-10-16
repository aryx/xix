(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: unix.mli,v 1.27 1997/08/29 15:37:19 xleroy Exp $ *)

(* Module [Unix]: interface to the Unix system *)

(*** Error report *)

type error =
  (* Errors defined in the POSIX standard *)
    E2BIG               (* Argument list too long *)
  | EACCES              (* Permission denied *)
  | EAGAIN              (* Resource temporarily unavailable; try again *)
  | EBADF               (* Bad file descriptor *)
  | EBUSY               (* Resource unavailable *)
  | ECHILD              (* No child process *)
  | EDEADLK             (* Resource deadlock would occur *)
  | EDOM                (* Domain error for math functions, etc. *)
  | EEXIST              (* File exists *)
  | EFAULT              (* Bad address *)
  | EFBIG               (* File too large *)
  | EINTR               (* Function interrupted by signal *)
  | EINVAL              (* Invalid argument *)
  | EIO                 (* Hardware I/O error *)
  | EISDIR              (* Is a directory *)
  | EMFILE              (* Too many open files by the process *)
  | EMLINK              (* Too many links *)
  | ENAMETOOLONG        (* Filename too long *)
  | ENFILE              (* Too many open files in the system *)
  | ENODEV              (* No such device *)
  | ENOENT              (* No such file or directory *)
  | ENOEXEC             (* Not an executable file *)
  | ENOLCK              (* No locks available *)
  | ENOMEM              (* Not enough memory *)
  | ENOSPC              (* No space left on device *)
  | ENOSYS              (* Function not supported *)
  | ENOTDIR             (* Not a directory *)
  | ENOTEMPTY           (* Directory not empty *)
  | ENOTTY              (* Inappropriate I/O control operation *)
  | ENXIO               (* No such device or address *)
  | EPERM               (* Operation not permitted *)
  | EPIPE               (* Broken pipe *)
  | ERANGE              (* Result too large *)
  | EROFS               (* Read-only file system *)
  | ESPIPE              (* Invalid seek e.g. on a pipe *)
  | ESRCH               (* No such process *)
  | EXDEV               (* Invalid link *)
  (* Additional errors, mostly BSD *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ELOOP               (* Too many levels of symbolic links *)
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR         (* Unknown error *)

        (* The type of error codes. *)

exception Unix_error of error * string * string
        (* Raised by the system calls below when an error is encountered.
           The first component is the error code; the second component
           is the function name; the third component is the string parameter
           to the function, if it has one, or the empty string otherwise. *)

external error_message : error -> string = "unix_error_message"
        (* Return a string describing the given error code. *)

val handle_unix_error : ('a -> 'b) -> 'a -> 'b
        (* [handle_unix_error f x] applies [f] to [x] and returns the result.
           If the exception [Unix_error] is raised, it prints a message
           describing the error and exits with code 2. *)


(*** Access to the process environment *)

external environment : unit -> string array = "unix_environment"
        (* Return the process environment, as an array of strings
           with the format ``variable=value''. *)
external getenv: string -> string = "sys_getenv"
        (* Return the value associated to a variable in the process
           environment. Raise [Not_found] if the variable is unbound.
           (This function is identical to [Sys.getenv].) *)
external putenv: string -> string -> unit = "unix_putenv"
        (* [Unix.putenv name value] sets the value associated to a
           variable in the process environment.
           [name] is the name of the environment variable,
           and [value] its new associated value. *)

(*** Process handling *)

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

        (* The termination status of a process. [WEXITED] means that the
           process terminated normally by [exit]; the argument is the return
           code. [WSIGNALED] means that the process was killed by a signal;
           the argument is the signal number. [WSTOPPED] means
           that the process was stopped by a signal; the argument is the
           signal number. *)

type wait_flag =
    WNOHANG
  | WUNTRACED

        (* Flags for [waitopt] and [waitpid].
           [WNOHANG] means do not block if no child has
           died yet, but immediately return with a pid equal to 0.
           [WUNTRACED] means report also the children that receive stop
           signals. *)

external execv : string -> string array -> unit = "unix_execv"
        (* [execv prog args] execute the program in file [prog], with
           the arguments [args], and the current process environment. *)
external execve : string -> string array -> string array -> unit = "unix_execve"
        (* Same as [execv], except that the third argument provides the
           environment to the program executed. *)
external execvp : string -> string array -> unit = "unix_execvp"
external execvpe : string -> string array -> string array -> unit = "unix_execvpe"
        (* Same as [execv] and [execvp] respectively, except that
           the program is searched in the path. *)
external fork : unit -> int = "unix_fork"
        (* Fork a new process. The returned integer is 0 for the child
           process, the pid of the child process for the parent process. *)
external wait : unit -> int * process_status = "unix_wait"
        (* Wait until one of the children processes die, and return its pid
           and termination status. *)
external waitpid : wait_flag list -> int -> int * process_status
           = "unix_waitpid"
        (* Same as [wait], but waits for the process whose pid is given.
           A pid of [-1] means wait for any child.
           A pid of [0] means wait for any child in the same process group
           as the current process.
           Negative pid arguments represent process groups.
           The list of options indicates whether [waitpid] should return
           immediately without waiting, or also report stopped children. *)
val system : string -> process_status
        (* Execute the given command, wait until it terminates, and return
           its termination status. The string is interpreted by the shell
           [/bin/sh] and therefore can contain redirections, quotes, variables,
           etc. The result [WEXITED 127] indicates that the shell couldn't
           be executed. *)
external getpid : unit -> int = "unix_getpid"
        (* Return the pid of the process. *)
external getppid : unit -> int = "unix_getppid"
        (* Return the pid of the parent process. *)
external nice : int -> int = "unix_nice"
        (* Change the process priority. The integer argument is added to the
           ``nice'' value. (Higher values of the ``nice'' value mean
           lower priorities.) Return the new nice value. *)

(*** Basic file input/output *)

type file_descr
        (* The abstract type of file descriptors. *)

val stdin : file_descr
val stdout : file_descr
val stderr : file_descr
        (* File descriptors for standard input, standard output and
           standard error. *)


type open_flag =
    O_RDONLY                            (* Open for reading *)
  | O_WRONLY                            (* Open for writing *)
  | O_RDWR                              (* Open for reading and writing *)
  | O_NONBLOCK                          (* Open in non-blocking mode *)
  | O_APPEND                            (* Open for append *)
  | O_CREAT                             (* Create if nonexistent *)
  | O_TRUNC                             (* Truncate to 0 length if existing *)
  | O_EXCL                              (* Fail if existing *)

        (* The flags to [open]. *)

type file_perm = int
        (* The type of file access rights. *)

external openfile : string -> open_flag list -> file_perm -> file_descr
   = "unix_open"
        (* Open the named file with the given flags. Third argument is
           the permissions to give to the file if it is created. Return
           a file descriptor on the named file. *)
external close : file_descr -> unit = "unix_close"
        (* Close a file descriptor. *)
val read : file_descr -> string -> int -> int -> int
        (* [read fd buff ofs len] reads [len] characters from descriptor
           [fd], storing them in string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually read. *)
val write : file_descr -> string -> int -> int -> int
        (* [write fd buff ofs len] writes [len] characters to descriptor
           [fd], taking them from string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually
           written. *)


(*** Interfacing with the standard input/output library. *)

external in_channel_of_descr : file_descr -> in_channel 
                             = "caml_open_descriptor"
        (* Create an input channel reading from the given descriptor. *)
external out_channel_of_descr : file_descr -> out_channel
                              = "caml_open_descriptor"
        (* Create an output channel writing on the given descriptor. *)
external descr_of_in_channel : in_channel -> file_descr = "channel_descriptor"
        (* Return the descriptor corresponding to an input channel. *)
external descr_of_out_channel : out_channel -> file_descr
                              = "channel_descriptor"
        (* Return the descriptor corresponding to an output channel. *)


(*** Seeking and truncating *)

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

        (* Positioning modes for [lseek]. [SEEK_SET] indicates positions
           relative to the beginning of the file, [SEEK_CUR] relative to
           the current position, [SEEK_END] relative to the end of the
           file. *)

external lseek : file_descr -> int -> seek_command -> int = "unix_lseek"
        (* Set the current position for a file descriptor *)
external truncate : string -> int -> unit = "unix_truncate"
        (* Truncates the named file to the given size. *)
external ftruncate : file_descr -> int -> unit = "unix_ftruncate"
        (* Truncates the file corresponding to the given descriptor
           to the given size. *)


(*** File statistics *)

type file_kind =
    S_REG                               (* Regular file *)
  | S_DIR                               (* Directory *)
  | S_CHR                               (* Character device *)
  | S_BLK                               (* Block device *)
  | S_LNK                               (* Symbolic link *)
  | S_FIFO                              (* Named pipe *)
  | S_SOCK                              (* Socket *)

type stats =
  { st_dev : int;                       (* Device number *)
    st_ino : int;                       (* Inode number *)
    st_kind : file_kind;                (* Kind of the file *)
    st_perm : file_perm;                (* Access rights *)
    st_nlink : int;                     (* Number of links *)
    st_uid : int;                       (* User id of the owner *)
    st_gid : int;                       (* Group id of the owner *)
    st_rdev : int;                      (* Device minor number *)
    st_size : int;                      (* Size in bytes *)
    st_atime : float;                   (* Last access time *)
    st_mtime : float;                   (* Last modification time *)
    st_ctime : float }                  (* Last status change time *)

        (* The informations returned by the [stat] calls. *)

external stat : string -> stats = "unix_stat"
        (* Return the information for the named file. *)
external lstat : string -> stats = "unix_lstat"
        (* Same as [stat], but in case the file is a symbolic link,
           return the information for the link itself. *)
external fstat : file_descr -> stats = "unix_fstat"
        (* Return the information for the file associated with the given
           descriptor. *)


(*** Operations on file names *)

external unlink : string -> unit = "unix_unlink"
        (* Removes the named file *)
external rename : string -> string -> unit = "unix_rename"
        (* [rename old new] changes the name of a file from [old] to [new]. *)
external link : string -> string -> unit = "unix_link"
        (* [link source dest] creates a hard link named [dest] to the file
           named [new]. *)


(*** File permissions and ownership *)

type access_permission =
    R_OK                                (* Read permission *)
  | W_OK                                (* Write permission *)
  | X_OK                                (* Execution permission *)
  | F_OK                                (* File exists *)

        (* Flags for the [access] call. *)

external chmod : string -> file_perm -> unit = "unix_chmod"
        (* Change the permissions of the named file. *) 
external fchmod : file_descr -> file_perm -> unit = "unix_fchmod"
        (* Change the permissions of an opened file. *) 
external chown : string -> int -> int -> unit = "unix_chown"
        (* Change the owner uid and owner gid of the named file. *) 
external fchown : file_descr -> int -> int -> unit = "unix_fchown"
        (* Change the owner uid and owner gid of an opened file. *) 
external umask : int -> int = "unix_umask"
        (* Set the process creation mask, and return the previous mask. *)
external access : string -> access_permission list -> unit = "unix_access"
        (* Check that the process has the given permissions over the named
           file. Raise [Unix_error] otherwise. *)


(*** Operations on file descriptors *)

external dup : file_descr -> file_descr = "unix_dup"
        (* Return a new file descriptor referencing the same file as
           the given descriptor. *)
external dup2 : file_descr -> file_descr -> unit = "unix_dup2"
        (* [dup2 fd1 fd2] duplicates [fd1] to [fd2], closing [fd2] if already
           opened. *)
external set_nonblock : file_descr -> unit = "unix_set_nonblock"
external clear_nonblock : file_descr -> unit = "unix_clear_nonblock"
        (* Set or clear the ``non-blocking'' flag on the given descriptor.
           When the non-blocking flag is set, reading on a descriptor
           on which there is temporarily no data available raises the
           [EAGAIN] or [EWOULDBLOCK] error instead of blocking;
           writing on a descriptor on which there is temporarily no room
           for writing also raises [EAGAIN] or [EWOULDBLOCK]. *)
external set_close_on_exec : file_descr -> unit = "unix_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "unix_clear_close_on_exec"
        (* Set or clear the ``close-on-exec'' flag on the given descriptor.
           A descriptor with the close-on-exec flag is automatically
           closed when the current process starts another program with
           one of the [exec] functions. *)


(*** Directories *)

external mkdir : string -> file_perm -> unit = "unix_mkdir"
        (* Create a directory with the given permissions. *)
external rmdir : string -> unit = "unix_rmdir"
        (* Remove an empty directory. *)
external chdir : string -> unit = "unix_chdir"
        (* Change the process working directory. *)
external getcwd : unit -> string = "unix_getcwd"
        (* Return the name of the current working directory. *)


type dir_handle

        (* The type of descriptors over opened directories. *)

external opendir : string -> dir_handle = "unix_opendir"
        (* Open a descriptor on a directory *)
external readdir : dir_handle -> string = "unix_readdir"
        (* Return the next entry in a directory.
           Raise [End_of_file] when the end of the directory has been 
           reached. *)
external rewinddir : dir_handle -> unit = "unix_rewinddir"
        (* Reposition the descriptor to the beginning of the directory *)
external closedir : dir_handle -> unit = "unix_closedir"
        (* Close a directory descriptor. *)


(*** Pipes and redirections *)

external pipe : unit -> file_descr * file_descr = "unix_pipe"
        (* Create a pipe. The first component of the result is opened
           for reading, that's the exit to the pipe. The second component is
           opened for writing, that's the entrance to the pipe. *) 

external mkfifo : string -> file_perm -> unit = "unix_mkfifo"
        (* Create a named pipe with the given permissions. *)


(*** High-level process and redirection management *)

val create_process :
  string -> string array -> file_descr -> file_descr -> file_descr -> int
        (* [create_process prog args new_stdin new_stdout new_stderr]
           forks a new process that executes the program
           in file [prog], with arguments [args]. The pid of the new 
           process is returned immediately; the new process executes
           concurrently with the current process.
           The standard input and outputs of the new process are connected
           to the descriptors [new_stdin], [new_stdout] and [new_stderr].
           Passing e.g. [stdout] for [new_stdout] prevents the redirection
           and causes the new process to have the same standard output
           as the current process.
           The executable file [prog] is searched in the path.
           The new process has the same environment as the current process.
           All file descriptors of the current process are closed in the
           new process, except those redirected to standard input and
           outputs. *)

val create_process_env :
  string -> string array -> string array -> 
  file_descr -> file_descr -> file_descr -> int
        (* [create_process_env prog args env new_stdin new_stdout new_stderr]
           works as [create_process], except that the extra argument
           [env] specifies the environment passed to the program. *)

val open_process_in: string -> in_channel
val open_process_out: string -> out_channel
val open_process: string -> in_channel * out_channel
        (* High-level pipe and process management. These functions
           run the given command in parallel with the program,
           and return channels connected to the standard input and/or
           the standard output of the command. The command is interpreted
           by the shell [/bin/sh] (cf. [system]). Warning: writes on channels
           are buffered, hence be careful to call [flush] at the right times
           to ensure correct synchronization. *)
val close_process_in: in_channel -> process_status
val close_process_out: out_channel -> process_status
val close_process: in_channel * out_channel -> process_status
        (* Close channels opened by [open_process_in], [open_process_out]
           and [open_process], respectively, wait for the associated
           command to terminate, and return its termination status. *)

(*** Symbolic links *)

external symlink : string -> string -> unit = "unix_symlink"
        (* [symlink source dest] creates the file [dest] as a symbolic link
           to the file [source]. *)
external readlink : string -> string = "unix_readlink"
        (* Read the contents of a link. *)


(*** Polling *)

external select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = "unix_select"

        (* Wait until some input/output operations become possible on
           some channels. The three list arguments are, respectively, a set
           of descriptors to check for reading (first argument), for writing
           (second argument), or for exceptional conditions (third argument).
           The fourth argument is the maximal timeout, in seconds; a
           negative fourth argument means no timeout (unbounded wait).
           The result is composed of three sets of descriptors: those ready
           for reading (first component), ready for writing (second component),
           and over which an exceptional condition is pending (third
           component). *)

(*** Locking *)

type lock_command =
    F_ULOCK               (* Unlock a region *)
  | F_LOCK                (* Lock a region, and block if already locked *)
  | F_TLOCK               (* Lock a region, or fail if already locked *)
  | F_TEST                (* Test a region for other process' locks *)

        (* Commands for [lockf]. *)

external lockf : file_descr -> lock_command -> int -> unit = "unix_lockf"

        (* [lockf fd cmd size] puts a lock on a region of the file opened
           as [fd]. The region starts at the current read/write position for
           [fd] (as set by [lseek]), and extends [size] bytes forward if
           [size] is positive, [size] bytes backwards if [size] is negative,
           or to the end of the file if [size] is zero. *)

(*** Signals *)

external kill : int -> int -> unit = "unix_kill"
        (* [kill pid sig] sends signal number [sig] to the process
           with id [pid]. *)
external pause : unit -> unit = "unix_pause"
        (* Wait until a non-ignored signal is delivered. *)


(*** Time functions *)

type process_times =
  { tms_utime : float;          (* User time for the process *)
    tms_stime : float;          (* System time for the process *)
    tms_cutime : float;         (* User time for the children processes *)
    tms_cstime : float }        (* System time for the children processes *)

        (* The execution times (CPU times) of a process. *)

type tm =
  { tm_sec : int;                       (* Seconds 0..59 *)
    tm_min : int;                       (* Minutes 0..59 *)
    tm_hour : int;                      (* Hours 0..23 *)
    tm_mday : int;                      (* Day of month 1..31 *)
    tm_mon : int;                       (* Month of year 0..11 *)
    tm_year : int;                      (* Year - 1900 *)
    tm_wday : int;                      (* Day of week (Sunday is 0) *)
    tm_yday : int;                      (* Day of year 0..365 *)
    tm_isdst : bool }                   (* Daylight time savings in effect *)

        (* The type representing wallclock time and calendar date. *)

external time : unit -> float = "unix_time"
        (* Return the current time since 00:00:00 GMT, Jan. 1, 1970,
           in seconds. *)
external gettimeofday : unit -> float = "unix_gettimeofday"
        (* Same as [time], but with resolution better than 1 second. *)
external gmtime : float -> tm = "unix_gmtime"
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes Greenwich meridian time zone. *)
external localtime : float -> tm = "unix_localtime"
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes the local time zone. *)
external mktime : tm -> float * tm = "unix_mktime"
        (* Convert a date and time, specified by the [tm] argument, into
           a time in seconds, as returned by [time]. Also return a normalized
           copy of the given [tm] record, with the [tm_wday] and [tm_yday]
           recomputed from the other fields. *)
external alarm : int -> int = "unix_alarm"
        (* Schedule a [SIGALRM] signals after the given number of seconds. *)
external sleep : int -> unit = "unix_sleep"
        (* Stop execution for the given number of seconds. *)
external times : unit -> process_times =
              "unix_times_bytecode" "unix_times_native"
        (* Return the execution times of the process. *)
external utimes : string -> float -> float -> unit = "unix_utimes"
        (* Set the last access time (second arg) and last modification time
           (third arg) for a file. Times are expressed in seconds from
           00:00:00 GMT, Jan. 1, 1970. *)

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF
        (* The three kinds of interval timers.
           [ITIMER_REAL] decrements in real time, and sends the signal
           [SIGALRM] when expired.
           [ITIMER_VIRTUAL] decrements in process virtual time, and sends
           [SIGVTALRM] when expired.
           [ITIMER_PROF] (for profiling) decrements both when the process
           is running and when the system is running on behalf of the
           process; it sends [SIGPROF] when expired. *)

type interval_timer_status =
  { it_interval: float;                 (* Period *)
    it_value: float }                   (* Current value of the timer *)
        (* The type describing the status of an interval timer *)

external getitimer: interval_timer -> interval_timer_status
   = "unix_getitimer" "unix_getitimer_native"
        (* Return the current status of the given interval timer. *)
external setitimer:
  interval_timer -> interval_timer_status -> interval_timer_status
  = "unix_setitimer" "unix_setitimer_native"
        (* [setitimer t s] sets the interval timer [t] and returns
           its previous status. The [s] argument is interpreted as follows:
           [s.it_value], if nonzero, is the time to the next timer expiration;
           [s.it_interval], if nonzero, specifies a value to
           be used in reloading it_value when the timer expires.
           Setting [s.it_value] to zero disable the timer.
           Setting [s.it_interval] to zero causes the timer to be disabled
           after its next expiration. *)

(*** User id, group id *)

external getuid : unit -> int = "unix_getuid"
        (* Return the user id of the user executing the process. *)
external geteuid : unit -> int = "unix_geteuid"
        (* Return the effective user id under which the process runs. *)
external setuid : int -> unit = "unix_setuid"
        (* Set the real user id and effective user id for the process. *)
external getgid : unit -> int = "unix_getgid"
        (* Return the group id of the user executing the process. *)
external getegid : unit -> int = "unix_getegid"
        (* Return the effective group id under which the process runs. *)
external setgid : int -> unit = "unix_setgid"
        (* Set the real group id and effective group id for the process. *)
external getgroups : unit -> int array = "unix_getgroups"
        (* Return the list of groups to which the user executing the process
           belongs. *)


type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string }
        (* Structure of entries in the [passwd] database. *)

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array }
        (* Structure of entries in the [groups] database. *)

external getlogin : unit -> string = "unix_getlogin"
        (* Return the login name of the user executing the process. *)
external getpwnam : string -> passwd_entry = "unix_getpwnam"
        (* Find an entry in [passwd] with the given name, or raise
           [Not_found]. *)
external getgrnam : string -> group_entry = "unix_getgrnam"
        (* Find an entry in [group] with the given name, or raise
           [Not_found]. *)
(*
external getpwuid : int -> passwd_entry = "unix_getpwuid"
        (* Find an entry in [passwd] with the given user id, or raise
           [Not_found]. *)
external getgrgid : int -> group_entry = "unix_getgrgid"
        (* Find an entry in [group] with the given group id, or raise
           [Not_found]. *)
*)

(*** Internet addresses *)

type inet_addr
        (* The abstract type of Internet addresses. *)

external inet_addr_of_string : string -> inet_addr
                                    = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "unix_string_of_inet_addr"
        (* Conversions between string with the format [XXX.YYY.ZZZ.TTT]
           and Internet addresses. [inet_addr_of_string] raises [Failure]
           when given a string that does not match this format. *)

val inet_addr_any : inet_addr
        (* A special Internet address, for use only with [bind], representing
           all the Internet addresses that the host machine possesses. *)

(*** Sockets *)

type socket_domain =
    PF_UNIX                             (* Unix domain *)
  | PF_INET                             (* Internet domain *)

        (* The type of socket domains. *)

type socket_type =
    SOCK_STREAM                         (* Stream socket *)
  | SOCK_DGRAM                          (* Datagram socket *)
  | SOCK_RAW                            (* Raw socket *)
  | SOCK_SEQPACKET                      (* Sequenced packets socket *)

        (* The type of socket kinds, specifying the semantics of
           communications. *)

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

        (* The type of socket addresses. [ADDR_UNIX name] is a socket
           address in the Unix domain; [name] is a file name in the file
           system. [ADDR_INET(addr,port)] is a socket address in the Internet
           domain; [addr] is the Internet address of the machine, and
           [port] is the port number. *)

external socket : socket_domain -> socket_type -> int -> file_descr
        = "unix_socket"
        (* Create a new socket in the given domain, and with the
           given kind. The third argument is the protocol type; 0 selects
           the default protocol for that kind of sockets. *)
external socketpair :
        socket_domain -> socket_type -> int -> file_descr * file_descr
        = "unix_socketpair"
        (* Create a pair of unnamed sockets, connected together. *)
external accept : file_descr -> file_descr * sockaddr = "unix_accept"
        (* Accept connections on the given socket. The returned descriptor
           is a socket connected to the client; the returned address is
           the address of the connecting client. *)
external bind : file_descr -> sockaddr -> unit = "unix_bind"
        (* Bind a socket to an address. *)
external connect : file_descr -> sockaddr -> unit = "unix_connect"
        (* Connect a socket to an address. *)
external listen : file_descr -> int -> unit = "unix_listen"
        (* Set up a socket for receiving connection requests. The integer
           argument is the maximal number of pending requests. *)

type shutdown_command =
    SHUTDOWN_RECEIVE                    (* Close for receiving *)
  | SHUTDOWN_SEND                       (* Close for sending *)
  | SHUTDOWN_ALL                        (* Close both *)
        (* The type of commands for [shutdown]. *)

external shutdown : file_descr -> shutdown_command -> unit = "unix_shutdown" 
        (* Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
           causes reads on the other end of the connection to return
           an end-of-file condition.
           [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
           to return a closed pipe condition ([SIGPIPE] signal). *)

external getsockname : file_descr -> sockaddr = "unix_getsockname"
        (* Return the address of the given socket. *)
external getpeername : file_descr -> sockaddr = "unix_getpeername"
        (* Return the address of the host connected to the given socket. *)

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK
        (* The flags for [recv], [recvfrom], [send] and [sendto]. *)

val recv : file_descr -> string -> int -> int -> msg_flag list -> int
val recvfrom :
        file_descr -> string -> int -> int -> msg_flag list -> int * sockaddr
        (* Receive data from an unconnected socket. *)
val send : file_descr -> string -> int -> int -> msg_flag list -> int
val sendto :
        file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int
        (* Send data over an unconnected socket. *)

type socket_option =
    SO_DEBUG               (* Record debugging information *)
  | SO_BROADCAST           (* Permit sending of broadcast messages *)
  | SO_REUSEADDR           (* Allow reuse of local addresses for bind *)
  | SO_KEEPALIVE           (* Keep connection active *)
  | SO_DONTROUTE           (* Bypass the standard routing algorithms *)
  | SO_OOBINLINE           (* Leave out-of-band data in line *)
        (* The socket options settable with [setsockopt]. *)

external getsockopt : file_descr -> socket_option -> bool = "unix_getsockopt"
        (* Return the current status of an option in the given socket. *)
external setsockopt : file_descr -> socket_option -> bool -> unit
                                                          = "unix_setsockopt"
        (* Set or clear an option in the given socket. *)

(*** High-level network connection functions *)

val open_connection : sockaddr -> in_channel * out_channel
        (* Connect to a server at the given address.
           Return a pair of buffered channels connected to the server.
           Remember to call [flush] on the output channel at the right times
           to ensure correct synchronization. *)
val shutdown_connection : in_channel -> unit
        (* ``Shut down'' a connection established with [open_connection];
           that is, transmit an end-of-file condition to the server reading
           on the other side of the connection. *)
val establish_server : (in_channel -> out_channel -> 'a) -> sockaddr -> unit
        (* Establish a server on the given address.
           The function given as first argument is called for each connection
           with two buffered channels connected to the client. A new process
           is created for each connection. The function [establish_server]
           never returns normally. *)


(*** Host and protocol databases *)

type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array }
          (* Structure of entries in the [hosts] database. *)

type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int }
          (* Structure of entries in the [protocols] database. *)

type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string }
          (* Structure of entries in the [services] database. *)

external gethostname : unit -> string = "unix_gethostname"
        (* Return the name of the local host. *)
external gethostbyname : string -> host_entry = "unix_gethostbyname"
        (* Find an entry in [hosts] with the given name, or raise
           [Not_found]. *)
external gethostbyaddr : inet_addr -> host_entry = "unix_gethostbyaddr"
        (* Find an entry in [hosts] with the given address, or raise
           [Not_found]. *)
external getprotobyname : string -> protocol_entry
                                         = "unix_getprotobyname"
        (* Find an entry in [protocols] with the given name, or raise
           [Not_found]. *)
external getprotobynumber : int -> protocol_entry
                                         = "unix_getprotobynumber"
        (* Find an entry in [protocols] with the given protocol number,
           or raise [Not_found]. *)
external getservbyname : string -> string -> service_entry
                                         = "unix_getservbyname"
        (* Find an entry in [services] with the given name, or raise
           [Not_found]. *)
external getservbyport : int -> string -> service_entry
                                         = "unix_getservbyport"
        (* Find an entry in [services] with the given service number,
           or raise [Not_found]. *)


(*** Terminal interface *)
(* pad: I removed it, plan9 has no such terminal *)

