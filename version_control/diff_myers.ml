(*s: version_control/diff_myers.ml *)
(*s: copyright ocaml-diff-myers *)
(*
 * Copyright (C) 2016 OOHASHI Daichi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)
(*e: copyright ocaml-diff-myers *)

(* was functorized and parametrized before *)
let equal = (=)

(*s: function Diff_myers.lcs *)
let lcs a b =
  let n = Array.length a in
  let m = Array.length b in
  let mn = m + n in
  let sz = 2 * mn + 1 in
  let vd = Array.make sz 0 in
  let vl = Array.make sz 0 in
  let vr = Array.make sz [] in
  let get v i = v.(i + mn) in
  let set v i x = v.(i + mn) <- x in
  let finish () =
    let rec loop i maxl r =
      match () with
      | _ when i > mn -> List.rev r
      | _ when get vl i > maxl -> loop (i + 1) (get vl i) (get vr i)
      | _ -> loop (i + 1) maxl r
    in loop (- mn) 0 []
  in
  if mn = 0 
  then []
  else
    (* For d <- 0 to mn Do *)
    let rec dloop d =
      assert (d <= mn);
      (* For k <- -d to d in steps of 2 Do *)
      let rec kloop k =
        if k > d 
        then dloop (d + 1)
        else
          let x, l, r =
            if k = -d || (k <> d && get vd (k - 1) < get vd (k + 1)) 
            then get vd (k + 1), get vl (k + 1), get vr (k + 1)
            else get vd (k - 1) + 1, get vl (k - 1), get vr (k - 1)
          in
          let x, y, l, r =
            let rec xyloop x y l r =
              if x < n && y < m && equal a.(x) b.(y)
              then xyloop (x + 1) (y + 1) (l + 1) (`Common(x, y, a.(x))::r)
              else x, y, l, r
            in xyloop x (x - k) l r
          in
          set vd k x;
          set vl k l;
          set vr k r;
          if x >= n && y >= m 
          then
            (* Stop *)
            finish ()
          else
            kloop (k + 2)
      in kloop (-d)
    in dloop 0
(*e: function Diff_myers.lcs *)

(*s: function Diff_myers.diff *)
let diff a b =
  let append_map g arr from to_ init =
    let rec loop i init =
      if i >= to_ 
      then init
      else loop (i + 1) (g i (arr.(i)):: init)
    in loop from init
  in
  let added   _i x = Diff.Added x in
  let removed _i x = Diff.Deleted x in
  let rec loop cs apos bpos init =
    match cs with
    | [] ->
        init
        |> append_map removed a apos (Array.length a)
        |> append_map added   b bpos (Array.length b)
    | `Common (aoff, boff, x) :: rest ->
        init
        |> append_map removed a apos aoff
        |> append_map added   b bpos boff
        |> (fun y -> (Diff.Equal x)::y)
        |> loop rest (aoff + 1) (boff + 1)
  in loop (lcs a b) 0 0 [] |> List.rev
(*e: function Diff_myers.diff *)
(*e: version_control/diff_myers.ml *)
