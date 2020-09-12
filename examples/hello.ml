open Flint

(* A type signifying the structure of our app *)
type app_state =
  { user_id : string
  ; user_authenticated : bool
  }

let json_handler ctx = ctx |> Web.Writers.as_json |> Web.ok "{\"message\":\"a json response\"}"

module User_handler = struct
  let show_user id ctx =
    let res = "Showing info for user " ^ id in
    ctx |> Web.ok res
end

let handlers = function
  | `GET, [ "" ] -> fun ctx -> ctx |> Web.ok "index page"
  | `GET, [ "hello" ] -> fun ctx -> Web.ok "Hello World" ctx
  | `GET, [ "users"; (_ as id); "show" ] -> User_handler.show_user id
  | `GET, [ "json" ] -> json_handler
  | _ -> fun ctx -> ctx |> Web.not_found "not found"


let () =
  let config : Flint.config = { port = 9000 } in
  let state = { user_id = "123"; user_authenticated = false } in
  Flint.simple_program config handlers state |> Lwt_main.run
