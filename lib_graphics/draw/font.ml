open Common

type t = {
  height : int;
}

let default_font () = {
  height = 10;
}

let fake_font = { height = -1 }


let string_width _font str =
  String.length str * 6
