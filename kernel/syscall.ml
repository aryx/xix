open Types

(* The generalized interface for syscall is:
 * syscall(char* bufin, int lenin, char* bufout, int lenout)
 * where we use marshall in and out for Syscall.request and Syscall.answer.
 * (like 9p does?)
 *)


(* Rfork *)

(* Rfork has a complex interface
 * less: opti: could use a bitset for the whole type
 *)
type rfork_flags = 
  | Fork of fork_flags * common_rfork_flags
  | NoFork of common_rfork_flags
  and fork_flags = {
    share_mem: bool;
    wait_child: bool;
  }
 and common_rfork_flags = {
    fork_fds: fork_kind;
    fork_namespace: fork_kind;
    fork_env: fork_kind;
    (* less: 
     * share_rendezvous_group: bool;
     * share_note_group: bool;
     *)
  }
  and fork_kind = Clean | Copy | Share (* Share mean Nothing for NoFork *)

(* Await *)

(* await result *)
type wait_msg = Proc_.wait_msg

(* Open *)

(* less: opti: a bitset *)
type open_flags = {
  open_read: bool;
  open_write: bool;
  (* less: 
   *  - open_exec: bool; (* imply open_read *) 
   *  - open_truncate: ...
   *  - open_close_on_exec:
   *  - open_remove_on_close
   *)
}

(* Stat *)

(* stat result *)
type dir_entry = unit (* todo: *)


module Request = struct
type t = 
  | Nop

  (* process *)
  | Rfork of rfork_flags
  | Exec of filename (* cmd *) * string list (* args *)
  | Await
  | Exits of string

  (* memory *)
  | Brk of user_addr

  (* file *)
  | Open of filename * open_flags
  | Close of fd
  | Pread of fd
  | Pwrite of fd
  | Seek of fd

  (* directory *)
  | Create of filename
  | Remove of filename
  | Chdir of filename
  | Fd2path of fd
  | Fstat of fd
  | Fwstat of fd
  (* less: Stat and Wstat are unecessary *)

  (* namespace *)
  | Bind
  | Mount
  | Umount

  (* time *)
  | Sleep of t_ms
  | Alarm of t_ms
  (* less: Nsec *)

  (* IPC *)
  | Pipe

  | Notify
  | Noted
  (* less: Segattach ... *)

  (* concurrency *)
  (*
  | RendezVous
  | SemAcquire
  | SemRelease
  *)
  (* less: TSemAcquire *)
  
  (* misc *)
  | Dup

  (* security *)
  (* less: Fversion | Fauth *) 
end

module Answer = struct
type t =
  | Void (* Nop, Exec, Exits, Brk? *)

  | Rfork of pid
  | Await of wait_msg

  | Error of string

end
