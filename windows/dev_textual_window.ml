open Common

open Device
module W = Window
module T = Terminal

let dev_text = { Device.default with
  name = "text";
  perm = Plan9.r;
  read_threaded = (fun offset count w ->
    let term = w.W.terminal in 
    let str = String.create term.T.nrunes in
    for i = 0 to term.T.nrunes - 1 do
      str.[i] <- term.T.text.(i);
    done;
    Device.honor_offset_and_count offset count str
  );
}


