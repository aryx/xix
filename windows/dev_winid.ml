open Common

open Device
module W = Window

let dev = { Device.default with
  name = "winid";
  perm = Plan9.r;
  read_threaded = (fun offset count w ->
    let str = spf "%11d" w.W.id in
    Device.honor_offset_and_count offset count str
  );
}


