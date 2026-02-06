
type t = {
  r: int8;
  g: int8;
  b: int8;
  a: int8;
}
and int8 = int

type rgba = t

val white: t
val black: t

val opaque: t
val transparent : t

val red: t
val green: t
val blue: t

val cyan: t
val magenta: t
val yellow: t

val darkgreen : t
val palegreen : t
val medgreen : t

val darkred : t
val palered : t
val medred : t

val greygreen : t
val palegreygreen : t

val nofill : t

val mk2 : int8 -> int8 -> int8 -> t


