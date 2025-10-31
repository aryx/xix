open Common
open Types

(* less: Pervasives would work? *)
let (@<) (VU a) (VU b) = a < b
let (@>) (VU a) (VU b) = a > b
let (@>=) (VU a) (VU b) = a >= b

(* safer pointer arithmetic *)
let (@-) (VU a) (VU b) = 
  assert (a >= b);
  a - b
let (@+) (VU a) b =
  assert (b > 0);
  VU (a+b)


let roundup_page (VU addr) = 
  VU (Int_.roundup addr Memory.pg2by)


let ok_addr_range _addr _len _write =
  (* less: every sections are writable for now. Just need
   * extra check and a Segment_.readonly for Kimage?
   *)
  (* todo: use Proc_segment.segment_of_addr *)
  raise Todo

let valid_addr_range _addr _len _write =
  raise Todo

let memchr _x =
  raise Todo
