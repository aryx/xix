
val thread: 
  int Event.channel * (* exit channel *) 
  Mouse.ctl * 
  (Display.t * Baselayer.t * Image.t * Font.t) * 
  Fileserver.t -> 
  unit
