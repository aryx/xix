open Common
open Point
open Rectangle

module D = Display
module I = Display (* Image.t is in display.ml *)
module M = Draw_marshal

open Display

(* less: public parameter *)
let name_image img name =
  let public = true in
  let len = String.length name in
  (* stricter: *)
  if len >= 256
  then failwith "long for image too long";

  let str = "N" ^ M.bp_long img.I.id ^ M.bp_bool public ^
    M.bp_byte len ^ name in
  Display.add_buf img.I.display str;
  (* will generate an exception if the name is already used *)
  Image.flush img

let get_named_image display name =
  let len = String.length name in
  if len >= 256
  then failwith "long for image too long";

  (* similar to Image.alloc_gen *)

  (* meh *)
  Display.flush_buffer display;

  display.D.imageid <- display.D.imageid + 1;
  let id = display.D.imageid in

  let str = "n" ^ M.bp_long id ^ M.bp_byte len ^ name in

  Display.add_buf display str;
  (* will generate an exception if the name is already used *)
  Display.flush display;

  (* similar to Display.init *)

  let ninfo = 12 * 12 in
  let str = String.make ninfo ' ' in

  let n = Unix1.read display.D.ctl str 0 ninfo in
  if n <> ninfo && 
     (* less: not sure why but it reads only 143 characters *)
     n <> (ninfo - 1)
  then failwith (spf "wrong format in /dev/draw/new; read %d chars (%s)" n str);

  let str_at n = 
    let s = String.sub str (n * 12) 12 in
    if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
    then Regexp.matched1 s
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

  let chans = Channel.channels_of_str (str_at 2) in
  let img =
  {
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
    baseid = None;
  }
  in
  assert (img.id = id);
  img
