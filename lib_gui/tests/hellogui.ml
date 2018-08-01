open Common

let main () =
  let _ = Graphics.open_graph " 500x500" in

  Graphics.loop_at_exit 
    [Graphics.Button_down;Graphics.Button_up;Graphics.Key_pressed]
    (fun status ->
      ()
    )


let _ = main ()
