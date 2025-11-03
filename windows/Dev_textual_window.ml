(*s: Dev_textual_window.ml *)
open Common

open Device
module W = Window
module T = Terminal

(*s: constant [[Dev_textual_window.dev_text]] *)
let dev_text = { Device.default with
  name = "text";
  perm = Plan9.r;
  read_threaded = (fun offset count w ->
    let term = w.W.terminal in 
    let str = Bytes.create term.T.nrunes in
    for i = 0 to term.T.nrunes - 1 do
      Bytes.set str i term.T.text.(i);
    done;
    Device.honor_offset_and_count offset count (Bytes.to_string str)
  );
}
(*e: constant [[Dev_textual_window.dev_text]] *)


(*e: Dev_textual_window.ml *)
