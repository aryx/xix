(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5
module T5 = Types5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ARM code generation.
 *
 * No need for optab/oplook/ocmp/cmp as in 5l. Just use pattern matching!
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let offset_to_R12 x =
  raise Todo

let error loc s =
  failwith (spf "%s at %s" s (T5.s_of_loc loc))

(*****************************************************************************)
(* Operand classes *)
(*****************************************************************************)

let immrot x =
  raise Todo

let immaddr x =
  raise Todo

(*****************************************************************************)
(* Code generation helpers *)
(*****************************************************************************)

(* more declaratif and give opportunity to sanity check if overlap *)
type composed_word = (int * int) list

(* gxxx below means gen_binary_code of xxx *)

let gcond cond =
  match cond with
  | EQ            -> (0x0, 28)
  | NE            -> (0x1, 28)
  | GE (Unsigned) -> (0x2, 28)
  | LT (Unsigned) -> (0x3, 28)
  | MI            -> (0x4, 28)
  | PL            -> (0x5, 28)
  | VS            -> (0x6, 28)
  | VC            -> (0x7, 28)
  | GT (Unsigned) -> (0x8, 28)
  | LE (Unsigned) -> (0x9, 28) 
  | GE (Signed)   -> (0xa, 28) 
  | LT (Signed)   -> (0xb, 28)
  | GT (Signed)   -> (0xc, 28)
  | LE (Signed)   -> (0xd, 28)
  | AL            -> (0xe, 28)
  | NV            -> (0xf, 28)


let gop_arith op opt =
  (match op with
  | AND -> [0x0, 21]
  | EOR -> [0x1, 21]
  | SUB -> [0x2, 21]
  | RSB -> [0x3, 21]
  | ADD -> [0x4, 21]
  | ADC -> [0x5, 21]
  | SBC -> [0x6, 21]
  | RSC -> [0x7, 21]
  (* TST 0x8, TEQ 0x9, CMP 0xa, CMN 0xb via gop_cmp below *)
  | ORR -> [0xc, 21]
  (* MOV 0xd *)
  | BIC -> [0xe, 21]
  | MVN -> [0xf, 21]

  | MUL | DIV | MOD -> raise (Impossible "should match those cases separately")
  | SLL | SRL | SRA -> raise (Impossible "should match those cases separately")
  ) @ 
  (match opt with
  | None -> []
  | Some Set_condition -> [(1, 20)]
  )

let gop_shift op =
  match op with
  | SLL -> (0, 5)
  | SRL -> (1, 5)
  | SRA -> (2, 5)
  | _ -> raise (Impossible "should match those cases separately")


let gop_cmp op =
  match op with
  | TST -> [(0x8, 21); (1, 20)]
  | TEQ -> [(0x9, 21); (1, 20)]
  | CMP -> [(0xa, 21); (1, 20)]
  | CMN -> [(0xb, 21); (1, 20)]

let gop_bitshift_register op =
  match op with
  | Sh_logic_left   -> (0x0, 5)
  | Sh_logic_right  -> (0x1, 5)
  | Sh_arith_right  -> (0x2, 5)
  | Sh_rotate_right -> (0x3, 5)

let gop_rcon x =
  match x with
  | Left (R r) -> [(r,8); (1, 4)]
  | Right i    -> [(i, 7); (0, 4)]

let gshift (R rf) op2 rcon = 
  gop_rcon rcon @ [gop_bitshift_register op2; (rf, 0)]

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)

type action = {
  size: int;
  pool: int option;
  binary: unit -> composed_word list;
}

(* conventions (matches the one used (inconsistently) in 5l):
 * - rf = register from (called Rm in refcard)
 * - rt = register to   (called Rd in refcard)
 * - r  = register middle (called Rn in refcard)
 *)
let rules symbols2 x =
  let loc = x.T5.loc in
  match x.T5.node with
  (* --------------------------------------------------------------------- *)
  (* Pseudo *)
  (* --------------------------------------------------------------------- *)

  (* TEXT instructions were kept just for better error reporting localisation *)
  | T5.TEXT (_, _, _) -> 
      { size = 0; pool = None; binary = (fun () -> []) }

  (* todo: actually write more rules with WORD instead of doing in aclass *)
  | T5.WORD x ->
      { size = 4; pool = None; binary = (fun () -> 
        match x with
        | Left i -> [ [(i, 0)] ]
        | Right (String s) -> raise Todo
        | Right (Address ent) -> raise Todo
        )
      }

  | T5.I (instr, cond) ->
    (match instr with

    (* --------------------------------------------------------------------- *)
    (* Arithmetics *)
    (* --------------------------------------------------------------------- *)
    | Arith ((AND|ORR|EOR|ADD|SUB|BIC|ADC|SBC|RSB|RSC|MVN) as op, opt, 
             from, middle, (R rt)) ->
        let r =
          if op = MVN
          then 0
          else 
            match middle with 
            | None -> rt 
            | Some (R x) -> x 
        in
        let from_part =
          match from with
          | Reg (R rf)      -> [(rf, 0)]
          | Shift (a, b, c) -> gshift a b c
          | Imm i ->
              (match immrot i with
              | Some (rot, v) -> [(1, 25); (rot, 8); (v, 0)]
              | None -> raise Todo
              )
        in
        { size = 4; pool = None; binary = (fun () ->
          [[gcond cond] @ gop_arith op opt @ [(r, 16); (rt, 12)] @ from_part]
        )}

    | Cmp (op, from, (R r)) ->
        let from_part = 
          match from with
          | Reg (R rf) -> [(rf, 0)]
          | Shift (a, b, c) -> gshift a b c
          | Imm i ->
              (match immrot i with
              | Some (rot, v) -> [(1, 25); (rot, 8); (v, 0)]
              | None -> raise Todo
              )
        in
        { size = 4; pool = None; binary = (fun () ->
          [[gcond cond] @ gop_cmp op @ [(r, 16); (0, 12)] @ from_part]
        )}
        
    | MOV (Word, None, Imsr from, Imsr (Reg (R rt))) -> 
        let from_part = 
          match from with
          | Reg (R rf) -> [(rf, 0)]
          | Shift (a, b, c) -> gshift a b c
          | Imm i ->
              (match immrot i with
              | Some (rot, v) -> [(1, 25); (rot, 8); (v, 0)]
              | None -> raise Todo
              )
        in
        { size = 4; pool = None; binary = (fun () ->
          [[gcond cond; (0xd, 21); (0, 16); (rt, 12)] @ from_part]
        )}

    | Arith ((SLL|SRL|SRA) as op, opt, from, middle, (R rt)) ->
        let r = 
          match middle with 
          | None -> rt 
          | Some (R x) -> x 
        in
        let from_part = 
          match from with
          | Imm i ->
              if i >= 0 && i <= 31
              then [(i, 7)]
              (* stricter: failwith, not silently truncate *)
              else error loc (spf "shit value out of range %d" i)
          | Reg (R rf) -> [(rf, 8); (1, 4)]
          (* stricter: I added that *)
          | Shift _ -> error loc "bitshift on shift operation not allowed"
        in
        { size = 4; pool = None; binary = (fun () ->
          [[gcond cond; (0xd, 21); (rt, 12); gop_shift op] @from_part@[(r,0)]]
        )}


    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* --------------------------------------------------------------------- *)
    (* Memory *)
    (* --------------------------------------------------------------------- *)

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* --------------------------------------------------------------------- *)
    (* Other *)
    (* --------------------------------------------------------------------- *)

    | _ -> error loc "illegal combination"
    )

(*****************************************************************************)
(* entry points *)
(*****************************************************************************)

let size_of_instruction symbols2 node =
  let action  = rules symbols2 node in
  action.size



let gen_one cg instr =
  raise Todo


(* TODO: double check pc is like one computed by layout_text 
   otherwise failwith  "phase error ..."
*)
let gen symbols cg =
  raise Todo


