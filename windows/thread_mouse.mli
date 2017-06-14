
val thread: 
  int Event.channel * 
  Mouse.ctl * 
  (Display.t * Baselayer.t * Image.t * Font.t) * 
  Fileserver.t -> 
  unit
