open Common
open Device

module F = File
module W = Window

(* The ancestor of rio (8 1/2) was serving a virtual /dev/draw,
 * which was more elegant but also more inefficient than the
 * /dev/winname (and associated /dev/draw/x/) approach used by rio.
 * 
 * alt: we could also pass the information of winname through 
 * the environment instead of through a /dev virtual file.
 *)

let dev_winname = { Device.default with
  name = "winname";
  perm = Plan9.r;
  
  read_threaded = (fun offset count w ->
    let str = w.W.winname in
    if str = ""
    then raise (Error "window has no name");
    Device.honor_offset_and_count offset count str
  );
}
