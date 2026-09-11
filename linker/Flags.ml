
(* set by '-debug_layout' (was set by ?? in 5l/vl/... ?) *)
let debug_layout = ref false
(* set by '-debug_gen' (was set by '-a' in 5l/vl/...) *)
let debug_gen = ref false

(* ??? *)
(* claude: was defaulting to true, which sets the MOV Rn field to the
 * destination register instead of 0; goken's 5l always encodes Rn=0
 * for MOV, and at least one ARM decoder (qemu-arm) actually SIGILLs
 * on the Rn=rt encoding, so default to goken's behavior here.
 *)
let kencc_compatible = ref false
