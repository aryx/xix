open Common

module P = Protocol_9P

let main () =
  let (clients_fd, server_fd) = Unix.pipe () in
  
  let pid = Unix.fork () in
  (match pid with
  (* child, client *)
  | 0 ->
    let res = Plan9.mount clients_fd (-1) "/mnt/wsys" Plan9.MRepl "1" in
    while true do
      ()
    done

  | pid ->
    while true do
      let msg = P.read_9P_msg server_fd in
      pr (P.str_of_msg msg)
    done
  )

let _ =
  main ()
