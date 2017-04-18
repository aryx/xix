open Types

(* less: a request type and an answer type? like plan9p? *)
type t = 
  | Nop

  (* process *)
  | Rfork of rfork_flags
  | Exec of filename * string list (* args *)
  | Await
  | Exits of string

  (* memory *)
  | Brk of user_addr

  (* file *)
  | Open of filename
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
  | Sleep of sec
  | Alarm of time
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

  (* todo? replace with a better error management comm between user/kernel?*)
  | Errstr


  (* Rfork has a complex interface
   * less: opti: could use a bitset for the whole type
   *)
  and rfork_flags = 
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
