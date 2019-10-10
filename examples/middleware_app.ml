open Flint

(* A type signifying the structure of our app *)
type app_state = {user_id: string; user_authenticated: bool}

(* Example with mdiddlewares *)
let just_log_something (ctx : 'a Web.http_context) =
  Logs.info (fun m -> m "just logging stuff") ;
  Some ctx |> Lwt.return

let user_name_begins_with_letter_a_middleware (ctx : 'a Web.http_context) =
  Logs.info (fun m -> m "Checking user id begginning with an 'A'") ;
  if ctx.state.user_id.[0] = 'a' then Some ctx |> Lwt.return
  else
    let ctx' = {ctx with continue= false} in
    Web.bad_request "string does not begin with a" ctx'

let happy (ctx : 'a Web.http_context) = Web.ok "we are happy" ctx

(* /combining middlewares into one single handler/ *)
let test_multis ctx =
  let open Web.Infix in
  let combined_handler =
    just_log_something <|> user_name_begins_with_letter_a_middleware <|> happy
  in
  combined_handler ctx

let routes =
  [ (`GET, "/hello", fun ctx -> Web.ok "Hello World" ctx)
  ; (`GET, "/test-multi-middlewares", test_multis) ]

let () =
  let config = {port= 9000} in
  let state = {user_id= "xxx"; user_authenticated= false} in
  Flint.program config routes state |> Lwt_main.run
