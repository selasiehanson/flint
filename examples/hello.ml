open Flint

(* A type signifying the structure of our app *)
type app_state = {user_id: string; user_authenticated: bool}

let json_example_handler ctx =
  ctx |> Web.Writers.as_json |> Web.ok "{\"message\":\"a json response\"}"

(* Use your json library here *)

let routes =
  [ (`GET, "/hello", fun ctx -> Web.ok "Hello World" ctx)
  ; (`GET, "/json-message", json_example_handler) ]

let () =
  let port = 9000 in
  let state = {user_id= "123"; user_authenticated= false} in
  Flint.program port routes state |> Lwt_main.run
