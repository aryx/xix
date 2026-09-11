
(* set by '-debug_layout' (was set by ?? in 5l/vl/... ?) *)
let debug_layout = ref false
(* set by '-debug_gen' (was set by '-a' in 5l/vl/...) *)
let debug_gen = ref false

(* ??? *)
(* claude: ARM-only (Codegen5.ml); other archs never read this. Was
 * defaulting to true, which sets the MOV Rn field to the
 * destination register instead of 0; goken's 5l always encodes Rn=0
 * for MOV, and at least one ARM decoder (qemu-arm) actually SIGILLs
 * on the Rn=rt encoding, so default to goken's behavior here.
 *)
let kencc_compatible = ref false

(* claude: ARM-only (Codegen5.ml's ArithF/CmpF); other archs never
 * read this. Set by '-f' (matches goken's 5l -f, `vfp = debug['f']`
 * in 5l/span.c): selects the VFP float encoding (cases 74/75,
 * opvfprrr) instead of the default legacy FPA/coprocessor-1 encoding
 * (case 54, oprrr). Off by default, matching goken. *)
let vfp = ref false
