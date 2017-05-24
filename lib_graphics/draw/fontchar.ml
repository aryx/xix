open Common

type t = {
  (* character coordinates in Subfont.bits *)

  (*/* left edge of bits */*)
  xleft_in_bits: int;
  (* /* first non-zero scan-line */ *)
  top: int;
  (* /* last non-zero scan-line + 1 */*)
  bottom: int;

  (* adjustments to make on drawing point coordinates in destination *)
  (* /* offset of baseline */ *)
  left: int;
  (* /* width of baseline */ *)
  width: int;
                             
}
