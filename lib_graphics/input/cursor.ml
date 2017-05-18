open Common

type byte = char

type t = {
  offset: Point.t;

  (* of size array_size *)
  (* less: opti: could be simply a string, but ocaml allows only
   * the syntax \232 and the cursor data is usually defined using
   * hexadecimal bytes so simpler to use a 'byte array'
   *)
  clr: byte array;
  set: byte array;
}

let array_size = 2 * 16

let ints_to_chars arr =
  arr |> Array.map (fun i ->
    if i < 0 || i > 255
    then failwith (spf "Cursor: wrong format, not a byte: %d" i);
    Char.chr i
  )
