type t =
  | TSpaces
  | TNewline
  | TEOF

  | TLetter of char
  | TInt of int
  | TString of string

  | TComma
  (* TODO: TPlus, TMinus, TPlusPlus, ... *)
