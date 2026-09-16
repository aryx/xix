(*s: Datagen.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fill_bytes_for_int (global : A.global) (arr : T.byte array) (base : int)
    (n : int) (bits: Arch.bits) (endian: Endian.t) :
    unit =

  let array_16, array_32, array_64 = Endian.array_functions_of_endian endian in

   (* claude: a negative `n` here isn't necessarily a real negative
    * *number* -- e.g. real 5c -S output for fmt/nan64.c's own
    * "uvneginf<>" (a raw IEEE754 -Inf bit pattern, sign bit set) uses
    * this same Int DATA path for what's really just a 32/64-bit
    * pattern, not a signed magnitude. Convert to the size's own
    * unsigned bit pattern via `land` (which, being a pure bitwise op,
    * is correct for any OCaml int regardless of sign -- unlike
    * split_16/split_32's `mod`, which follows the *dividend*'s sign
    * in OCaml, i.e. is wrong for negative input; masking to a
    * guaranteed-non-negative value first sidesteps that) before
    * handing off to array_16/array_32, rather than rejecting negative
    * input outright as before. Found stress-testing against real
    * lib_core/libc -- see
    * docs/claude_notes/plan_hello_libc_linking.md. *)
   match bits with
   | Arch.Arch8 when n >= -0x80 && n <= 0xff ->
      arr.(base) <- Char.chr (n land 0xff)
   | Arch.Arch16 when n >= -0x8000 && n <= 0xffff ->
      array_16 (n land 0xffff) |> Array.iteri (fun i el -> arr.(base + i) <- el)
   | Arch.Arch32 when n >= -0x80000000 && n <= 0xffffffff ->
      array_32 (Int32.of_int n) |> Array.iteri (fun i el -> arr.(base + i) <- el)
   (* claude: needed for ARM64 (and any other 64-bit arch) DATA
    * statements with an 8-byte int slice, e.g. a plain integer global
    * -- the upper bound this project's other size cases check (e.g.
    * Arch32's 0xffffffff) isn't meaningfully expressible here; any
    * `n` representable as an OCaml native int at all (on whatever
    * host this happens to be compiled for) already fits within 8
    * bytes. *)
   | Arch.Arch64 ->
      array_64 (Int64.of_int n) |> Array.iteri (fun i el -> arr.(base + i) <- el)
   | _ ->
      failwith (spf "int for %s < 0 or too big for its size"
                            (A.s_of_global global))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Datagen.gen]] *)
let gen (symbols2 : T.symbol_table2) (init_data : T.addr)
        (sizes : Exec_file.sections_size) (endian: Endian.t)
        (ds : T.data list) :
        T.byte array =
  let arr : T.byte array =
    Array.make sizes.data_size (Char.chr 0) in

  ds |> List.iter (fun d ->
      (* alt: have a Flags.debug_data *)
    Logs.debug (fun m -> m "Datagen.gen for %s" (T.show_data d));
      
    let T.DATA (global, offset2, size_slice, v) = d in
    let info = Hashtbl.find symbols2 (T.symbol_of_global global) in
    match info with
    | T.SData2 (offset, T.Data) ->
        let base = offset + offset2 in

        (match v with
        | A.Int n ->
            (match size_slice with
            (* claude: added 8 -- a real, needed size on 64-bit archs
             * (ARM64's own literal-pool WORD entries for an
             * address-of-global or big constant are 8 bytes, going
             * through this same DATA-writing path when the pool is
             * flushed -- see Layout7.ml/Codegen7.ml). *)
            | 1 | 2 | 4 | 8 ->
               fill_bytes_for_int global arr base n
                        (Arch.bits_of_intsize size_slice) endian
            | _ ->
                failwith (spf "size for %s not in {1,2,4,8}"
                            (A.s_of_global global));
            )

        (* claude: a real float/double constant in the DATA segment,
         * e.g. real 5c -S output for fmt/fltfmt.c's "pows10<>" table
         * (goken's own precomputed powers-of-ten table for %e/%g
         * float formatting): "DATA pows10<>+8(SB)/8,$1.0e+01" etc.
         * Just the raw IEEE754 bit pattern, same byte-splitting as
         * the Int case above (Bits_of_float.bits_of_float{32,64} give
         * the bit pattern as an Int32.t/Int64.t, which array_64/
         * array_32 then split into bytes the same way regardless of
         * what the bits actually mean). Found stress-testing against
         * real lib_core/libc -- see
         * docs/claude_notes/plan_hello_libc_linking.md.
         * NOTE: a negative double's sign bit (bit 63) used to get
         * silently dropped here (narrowing to a plain int lost it) --
         * now that array_64 takes an Int64.t directly,
         * bits_of_float64's own result flows straight through, no
         * narrowing needed. *)
        | A.Float f ->
            let (_array_16, array_32, array_64) =
              Endian.array_functions_of_endian endian in
            (match size_slice with
            | 4 ->
                array_32 (Bits_of_float.bits_of_float32 f)
                |> Array.iteri (fun i el -> arr.(base + i) <- el)
            | 8 ->
                array_64 (Bits_of_float.bits_of_float64 f)
                |> Array.iteri (fun i el -> arr.(base + i) <- el)
            | _ ->
                failwith (spf "float size for %s not in {4,8}"
                            (A.s_of_global global))
            )

        | A.String s ->
            if size_slice > 8 
            then failwith (spf "size for %s > 8" (A.s_of_global global));

            for i = 0 to size_slice -1 do 
              arr.(base + i) <- s.[i] 
            done

        | A.Address (A.Global (global2, offset_global)) ->
            let info2 = Hashtbl.find symbols2 (T.symbol_of_global global2) in
            let n =
              match info2 with
              | T.SText2 real_pc -> real_pc
              | T.SData2 (offset, _kind) -> init_data + offset
            in
            (* claude: a real "DATA sym+N(SB)/4,$other_sym+M(SB)"
             * (address of a global *plus an offset*, e.g. a pointer
             * into the middle of an array/struct rather than its
             * start), found stress-testing against real
             * lib_core/libc -- see
             * docs/claude_notes/plan_hello_libc_linking.md. Was
             * asserted always 0 before; just add it to the resolved
             * base address like any other address computation. *)
            let n = n + offset_global in
            (* TODO: what about 64 bits arch? *)
            fill_bytes_for_int global (* not global2*) arr base n
                  Arch.Arch32 endian

        | (A.Address (A.Local _ | A.Param _)) ->
            raise (Impossible "address of local or param in DATA")
        )

    | T.SData2 (_, T.Bss) -> raise (Impossible "layout_data missed a DATA")
    | T.SText2 _ -> raise (Impossible "layout_data did this check")
  );

  (* TODO? -debug_data and output its content? *)
  arr
(*e: function [[Datagen.gen]] *)
(*e: Datagen.ml *)
