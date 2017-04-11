open Types

(* todo: for arm *)
type t = {
  r0: int32;
  r1: int32;
  r2: int32;
  r3: int32;
  r4: int32;
  r5: int32;
  r6: int32;
  r7: int32;
  r8: int32;
  r9: int32;
  r10: int32;
  r11: int32;

  (* SB *)
  r12: int32;
  (* SP *)
  r13: int32;
  (* LINK *)
  r14: int32;
  (* PC *)
  r15: int32;

  psr: int32;
}
