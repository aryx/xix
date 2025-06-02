(*s: Prompt.mli *)
(*s: signature [[Prompt.doprompt]] *)
val doprompt : bool ref
(*e: signature [[Prompt.doprompt]] *)
(*s: signature [[Prompt.prompt]] *)
val prompt : string ref
(*e: signature [[Prompt.prompt]] *)

(*s: signature [[Prompt.pprompt]] *)
(* !will reset doprompt! *)
val pprompt : unit -> unit
(*e: signature [[Prompt.pprompt]] *)
(*e: Prompt.mli *)
