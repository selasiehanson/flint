open Flint

(* A type signifying the structure of our app *)
type app_state = {user_id: string; user_authenticated: bool}

let routes = [
    (`GET, "/hello", (fun ctx -> Web.ok "Hello World" ctx))
]


let () =
  let port = 9000 in
  let state = {user_id= "123"; user_authenticated= false} in
  Flint.program port routes state |> Lwt_main.run
