open Common
open Types
open Process_
open User_memory (* for the @< operators *)


(* todo: factorize with plan9-ml/formats/executable/a_out.ml? *)
module A_out = struct

(* note that a.out uses big-endian integers even on low-endian architectures*)
type header = {
  (* see Arch.aout_magic for the arch-specific magic number to check *)
  magic: int32;

  text_size: int32;
  data_size: int32;
  bss_size: int32;

  symbol_size: int32;
  pc_size: int32;

  entry: user_addr; (* int32 *)
}
let sizeof_header = 0x20 (* 32 *)

(* may raise Error.Ebadexec *)
let parse_header str = 
  assert (String.length str = sizeof_header);
  (* todo: do the int32_from_big_endian *)
  raise Todo

end

let syscall_exec (cmd : string) _args =
  let up = Globals.up () in

  (* big call *)
  let chan = !Hooks.Chan.chan_of_filename cmd in
  Fun.protect ~finally:(fun () -> !Hooks.Chan.close chan) (fun () ->

    let dev = !Globals.devtab.(chan.Chan_.chantype) in
    let buf = String.make A_out.sizeof_header ' ' in
    (* big call *)
    let n = dev.Device_.read chan buf A_out.sizeof_header (Device_.Seek 0) in

    (* sanity checks *)
    if n < 2
    then Error.error Error.Ebadexec;
    (* less: handle #! *)
    if n < A_out.sizeof_header
    then Error.error Error.Ebadexec;

    let header = A_out.parse_header buf in
    if header.A_out.magic <> Arch.aout_magic ||
       header.A_out.text_size >= (Memory.ustktop @- Memory.utzero) ||
       header.A_out.entry @< (Memory.utzero @+ A_out.sizeof_header) ||
       header.A_out.entry @>= 
         (Memory.utzero @+ (A_out.sizeof_header + header.A_out.text_size))
    then Error.error Error.Ebadexec;

    let end_text = User_memory.roundup_page
      (Memory.utzero @+ (A_out.sizeof_header + header.A_out.text_size))
    in
    let end_data = User_memory.roundup_page
      (end_text @+ header.A_out.data_size)
    in
    (* note that this is not end_data + bss_size! The BSS follows
     * directly the data. It will use the bytes from the end of the
     * data segment (hence special code in sysbrk)
     *)
    let end_bss = User_memory.roundup_page
      (end_text @+ (header.A_out.data_size + header.A_out.bss_size))
    in

    if end_text @>= (VU Arch.kzero) || 
       end_data @>= (VU Arch.kzero) || 
       end_bss  @>= (VU Arch.kzero)
    then Error.error Error.Ebadexec;

    (* less: build args count? *)
    (* todo: build temporary stack segment *)
    (* todo: build args *)

    (* free old segments *)
    up.seg |> Hashtbl.iter (fun _section s ->
        (* less: do not free special segments?
         * less: what about seglock?
         *)
      Segment.free s;
    );
    Hashtbl.clear up.seg;

    (* set new segments *)
    let text_segment = 
      (* todo: from tc, demand loading on channel tc *)
      raise Todo 
    in
    Hashtbl.add up.seg Process_.SText text_segment;

    let data_segment = 
      Segment.alloc Segment_.SData end_text 
        (((end_data @- end_text) lsr Memory.pgshift))
    in
    (* todo: demand loading on tc too *)
    Hashtbl.add up.seg Process_.SData data_segment;

    let bss_segment =
      Segment.alloc Segment_.SBss end_data
        (((end_bss @- end_data) lsr Memory.pgshift))
    in
    Hashtbl.add up.seg Process_.SBss bss_segment;

    (* todo: stack segment, stack segment relocation *)

    (* todo: arch_flushmmu, arch_procsetup, arch_execregs *)
    let _ = raise Todo in
    ()

   )

