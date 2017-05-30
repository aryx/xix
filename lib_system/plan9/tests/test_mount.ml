open Common

let main () =
  let (clients_fd, server_fd) = Unix.pipe () in
  
  let pid = Unix.fork () in
  (match pid with
  (* child, client *)
  | 0 ->
    let res = Plan9.mount clients_fd (-1) "/mnt/wsys" Plan9.mREPL "1" in
    while true do
      ()
    done

  | pid ->
    let buf = String.make 1024 ' ' in
    while true do
      let n = Unix.read server_fd buf 0 1024 in
      pr (spf "S: read %d, str = %s" n (String.sub buf 0 n));
    done
  )

    

let _ =
  main ()
