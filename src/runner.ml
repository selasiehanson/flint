open Lwt.Infix
open Httpaf_lwt_unix

type config = { port : int }

let setup_logger () =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Fmt_tty.setup_std_outputs () ;
  Logs.set_level (Some Logs.Debug) ;
  Logs.set_reporter (Logs_fmt.reporter ())


let make_custom_handler routes = Web.make_router routes

let program config routes (application_state : 'a) =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, config.port)) in
  Lwt.async (fun () ->
      let _r_h = make_custom_handler routes application_state in
      Lwt_io.establish_server_with_client_socket
        listen_address
        (Server.create_connection_handler
           ~request_handler:(make_custom_handler routes application_state)
           ~error_handler:Web.error_handler)
      >|= fun _server ->
      setup_logger () ;
      Stdio.printf "Listening on port :%d\n\n%!" config.port) ;
  let forever, _ = Lwt.wait () in
  forever
