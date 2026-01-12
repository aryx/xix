(*s: version_control/clients.mli *)

(*s: signature [[Clients.client_of_url]] *)
val client_of_url: 
  < Cap.open_in; Cap.open_out; ..> ->
  string -> Client.t
(*e: signature [[Clients.client_of_url]] *)
(*e: version_control/clients.mli *)
