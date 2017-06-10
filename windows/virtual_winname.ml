open Common
open Device

module F = File
module W = Window

let dev = { Device.default with
  name = "winname";
  perm = Plan9.r;

  read_threaded = (fun offset count w ->
    let str = w.W.winname in
    let str = 
      if str = ""
      then raise (Error "window has no name")
      else str
    in
    Device.honor_offset_and_count offset count str
  );
}

