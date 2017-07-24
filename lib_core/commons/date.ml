open Common

(* Minimalist library around Unix.tm.
 * 
 * alternatives:
 *  - ??
 *)

(* less: i18n *)
let string_of_day = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | x -> raise (Impossible (spf "string_of_day not in range %d" x))

let string_of_month = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | x -> raise (Impossible (spf "string_of_month not in range %d" x))
