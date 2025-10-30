
type item = (string * (unit -> unit))
type items = item list

val menu: 
  items -> 
  Point.t (* position of menu *) -> 
  Mouse.button (* which button release to look for*) ->
  Mouse.ctl -> (Display.t * Baselayer.t * Image.t * Font.t) ->
  unit

