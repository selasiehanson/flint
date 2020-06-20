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

(* let simple_request_handler reqd = *)
(* let open Httpaf in *)
(* let { Httpaf.Request.meth; target; _ } = Httpaf.Reqd.request reqd in *)
(* match meth with *)
(* | `GET -> *)
(* ( match String.split_on_char '/' target with *)
(* | "" :: "hello" :: rest -> *)
(* let who = match rest with [] -> "world" | x :: _ -> x in *)
(* let response_body = Printf.sprintf "Hello, %s" who in *)
(* let headers = *)
(* Headers.of_list [ ("Content-length", Int.to_string (String.length response_body)) ] *)
(* in *)
(* Reqd.respond_with_string reqd (Response.create ~headers `OK) response_body *)
(* | _ -> *)
(* let response_body = Printf.sprintf "%S not found\n" target in *)
(* invalid_request reqd `Not_found response_body ) *)
(* | meth -> *)
(* let response_body = Printf.sprintf "%s is not an allowed method\n" (Method.to_string meth) in *)
(* invalid_request reqd `Method_not_allowed response_body *)
