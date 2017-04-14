open Types

type t = 
  | Nop

  (* process *)
  | Rfork
  | Exec of filename
  | Exits of string
  | Await

  (* memory *)
  | Brk of user_addr option

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
