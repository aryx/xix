open Common
open Point
open Rectangle

open Image (* todo: delete once can do simplified qualified record *)

type t = Image.display

let flush display =
  Image.flush_display display


(* less: devdir? windir? errorfn? *)
let init () =
  (* less: OCEXEC *)
  let ctlfd = 
    (* less: could use finalize to close if exn at least *)
    Unix.openfile "/dev/draw/new" [Unix.O_RDWR] 0o666 in

  let ninfo = 12 * 12 in
  let str = String.make ninfo ' ' in

  let n = Unix.read ctlfd str 0 ninfo in
  if n <> ninfo && 
     (* less: not sure why but it reads only 143 characters *)
     n <> (ninfo - 1)
  then failwith (spf "wrong format in /dev/draw/new; read %d chars (%s)" n str);

  let str_at n = 
    let s = String.sub str (n * 12) 12 in
    if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
    then Common.matched1 s
    else failwith (spf "not a /dev/draw/new entry, got %s" s)
  in
  let int_at n = 
    try int_of_string (str_at n)
    with Failure _ -> failwith (spf "not an int at %d (%s)" n (str_at n))
  in
  let bool_at n = 
    match int_at n with 
    | 0 -> false 
    | 1 -> true 
    | x -> failwith (spf "wrong format for boolean, accept 0 or 1 not %d" x)
  in

  let clientnb = int_at 0 in

  let datafd = 
    Unix.openfile (spf "/dev/draw/%d/data" clientnb) [Unix.O_RDWR] 0o666 in
  (* less: refreshfd *)

  let chans = Channel.channels_of_str (str_at 2) in

  let rec image = {
    id = int_at 1;
    chans = chans;
    depth = Channel.depth_of_channels chans;
    repl = bool_at 3;
    r = { min = { x = int_at 4; y = int_at 5 }; 
          max = { x = int_at 6; y = int_at 7 };
        };
    clipr = { min = { x = int_at 8; y = int_at 9 }; 
              max = { x = int_at 10; y = int_at 11 };
            };
    display = display;
  }
  and display = {
    dirno = clientnb;
    ctl = ctlfd;
    data = datafd;

    image = image;
    imageid = 0;
    (* +1 for space for 'v' when flush_display() *)
    buf = String.make (Image.bufsize + 1) ' ';  
    bufp = 0;

  }
  in
  assert(image.id = 0);

  (* todo: allocimage here? *)
  display
