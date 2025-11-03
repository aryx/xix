(*s: Dev_wm.ml *)
open Common

open Device
module W = Window

(*s: constant [[Dev_wm.dev_winid]] *)
let dev_winid = { Device.default with
  name = "winid";
  perm = Plan9.r;
  read_threaded = (fun offset count w ->
    let str = spf "%11d" w.W.id in
    Device.honor_offset_and_count offset count str
  );
}
(*e: constant [[Dev_wm.dev_winid]] *)
(*e: Dev_wm.ml *)
