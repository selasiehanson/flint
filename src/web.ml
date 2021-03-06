open Core
open Httpaf
module String = Core.String

type http_request =
  { meth : Httpaf.Method.t
  ; path : string
  ; headers : Httpaf.Headers.t
  ; body : string option
  ; path_params : (string * string) list
  ; query_params : (string * string) list
  }

type http_response =
  { status_code : Status.t
  ; body : string
  ; headers : (string * string) list
  }

let make_request meth path headers body =
  { meth; path; headers; body; path_params = []; query_params = [] }


let make_response status_code headers body = { status_code; body; headers }

type 'a http_context =
  { request : http_request
  ; response : http_response
  ; state : 'a
  ; continue : bool
  }

type 'a server = 'a http_context -> 'a http_context option Lwt.t

let middleware_combine first_filter second_filter ctx =
  let%lwt result1 = first_filter ctx in
  let res =
    match result1 with
    | None -> None |> Lwt.return
    | Some first_result_ctx ->
        if first_result_ctx.continue
        then second_filter first_result_ctx
        else Some first_result_ctx |> Lwt.return
  in
  res


module Infix = struct
  let ( <|> ) = middleware_combine
end

include Infix

(* Response Helpers *)
let bad_request body (ctx : 'a http_context) =
  let modified_response = { ctx.response with body; status_code = `Bad_request } in
  Some { ctx with response = modified_response } |> Lwt.return


let ok (body : string) (ctx : 'a http_context) =
  let modified_response = { ctx.response with body; status_code = `OK } in
  Some { ctx with response = modified_response } |> Lwt.return


let unauthorized message (ctx : 'a http_context) =
  let modified_response = { ctx.response with body = message; status_code = `Unauthorized } in
  Some { ctx with response = modified_response } |> Lwt.return


let not_found message (ctx : 'a http_context) =
  let modified_response = { ctx.response with body = message; status_code = `Not_found } in
  Some { ctx with response = modified_response } |> Lwt.return


let internal_server_error message (ctx : 'a http_context) =
  let modified_response =
    { ctx.response with body = message; status_code = `Internal_server_error }
  in
  Some { ctx with response = modified_response } |> Lwt.return


module Writers = struct
  let as_mime mime ctx =
    let old_headers = ctx.response.headers in
    let updated_headers = old_headers @ [ ("Content-Type", mime) ] in
    let new_ctx = { ctx with response = { ctx.response with headers = updated_headers } } in
    new_ctx


  let as_json ctx = as_mime "application/json" ctx

  let as_html ctx = as_mime "text/html" ctx
end

module Readers = struct
  let from_params (string_key : string) ctx =
    let params : (string * string) list = ctx.request.path_params in
    List.find
      ~f:(fun p ->
        let (key' : string), _ = p in
        String.(string_key = key'))
      params
end

module Router = struct
  (* Router docs *)
  (* [(`GET, "users/:id" , User.get ) *)
  (* ;(`GET, "greet/hello" , (fun ctx -> Web.ok ctx) )] *)

  let remove_empty_strings list = List.filter ~f:(fun t -> String.(t = "") |> not) list

  (* Given a string such as :name return name *)
  let pull_path_param str =
    if String.length str > 1 then Caml.String.sub str 1 (String.length str - 1) else str


  (* Taking a string such as this split into tuples "a=q&b=c&d=1" *)
  let parse_query_params str =
    let pairs = String.split str ~on:'&' in
    let param_pairs =
      List.map pairs ~f:(fun pair ->
          match String.split pair ~on:'=' with [ key; value ] -> (key, value) | _ -> ("", ""))
    in
    List.filter param_pairs ~f:(fun (k, v) -> not String.(k = "" && v = ""))


  (*remove the empty pair*)

  let rec parse_whiles_going_right structured_list path_segment_list extracted_path_params_list =
    match (List.hd structured_list, List.hd path_segment_list) with
    | Some _, None -> (false, [])
    | None, Some _ -> (false, [])
    | None, None -> (true, extracted_path_params_list)
    | Some (a : string), Some (b : string) ->
        if String.(a = b) && List.length structured_list = 1
        then (true, extracted_path_params_list)
        else if String.(a = b)
        then
          let path_param_pair = (pull_path_param a, b) in
          parse_whiles_going_right
            (List.drop structured_list 1)
            (List.drop path_segment_list 1)
            (extracted_path_params_list @ [ path_param_pair ])
        else if String.contains a ':'
        then
          let path_param_pair = (pull_path_param a, b) in
          parse_whiles_going_right
            (List.drop structured_list 1)
            (List.drop path_segment_list 1)
            (extracted_path_params_list @ [ path_param_pair ])
        else (false, [])


  let parse_route (route_format : string) (in_url : string) (ctx : 'a http_context) =
    (* todo strip slashes at the ends of url *)
    let url, _query_params =
      match String.index in_url '?' with
      | Some pos ->
          let path_segment = Caml.String.sub in_url 0 pos in
          let query_params_str = Caml.String.sub in_url pos (String.length in_url) in
          (path_segment, Some query_params_str)
      | None -> (in_url, None)
    in
    (* break the route STRUCTURE we want to parse into segements *)
    (* eg "invoices/:id/report" becomes ["invoices"; ":id"; "report"] *)
    let broken_structure = String.split ~on:'/' route_format |> remove_empty_strings in
    (* break the incoming url into parts *)
    (* eg "invoices/1/report" becomes ["invoices"; "1"; "report"] *)
    let broken_url = String.split ~on:'/' url |> remove_empty_strings in
    match List.length broken_structure = List.length broken_url with
    | false -> (false, ctx) (* return false lenghts are not the same *)
    | true ->
        let is_match, path_params = parse_whiles_going_right broken_structure broken_url [] in
        if is_match
        then
          let updated_context = { ctx with request = { ctx.request with path_params } } in
          (true, updated_context)
        else (false, ctx)
end

let get_content_length s = String.length s

let respond_with_text reqd status result =
  let headers =
    Headers.of_list
      ([ ("content-length", string_of_int (get_content_length result.body)) ] @ result.headers)
  in
  Reqd.respond_with_string reqd (Response.create ~headers status) result.body


let make_ctx app_state meth path body initial_headers =
  let initial_ctx =
    { request = make_request meth path initial_headers body
    ; response = make_response `Not_found [] ""
    ; state = app_state
    ; continue = true
    }
  in
  initial_ctx


let log_request req (body : string) =
  let time_of_request = Time.now () |> Time.to_string in
  let meth_as_string = Httpaf.Method.to_string req.Request.meth in
  let path = req.target in
  Logs.info (fun m -> m "%s - %s %s" time_of_request meth_as_string path) ;
  if String.(body <> "") then Logs.info (fun m -> m "%s" body)


let read_request_body reqd =
  let next, awake = Lwt.wait () in
  Lwt.async (fun () ->
      let request_body = Reqd.request_body reqd in
      let temp_str = ref "" in
      let rec on_read buffer ~off ~len =
        let read = Bigstringaf.substring ~off ~len buffer in
        temp_str := !temp_str ^ read ;
        Body.schedule_read request_body ~on_eof ~on_read
      and on_eof () = Lwt.wakeup_later awake temp_str.contents in
      Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read ;
      Lwt.return_unit) ;
  next


(*TODO Remove path parameters *)
(* Remove the begginning and trailing slashes if the exist *)
let path_from_url url =
  let slash_strip_fn c = Char.(c = '/') in
  url |> String.lstrip ~drop:slash_strip_fn |> String.rstrip ~drop:slash_strip_fn


let handle_request route handlers meth =
  let route_path = path_from_url route in
  let parts = String.split route_path ~on:'/' in
  handlers (meth, parts)


let make_simple_handler handlers app_state =
  let request_handler _ reqd =
    let open Lwt.Infix in
    read_request_body reqd
    >|= (fun body ->
          log_request (Reqd.request reqd) body ;
          let req = Reqd.request reqd in
          let initial_ctx =
            make_ctx app_state req.Request.meth req.target (Some body) req.headers
          in
          let handle_fn = handle_request req.target handlers initial_ctx.request.meth in
          let out =
            handle_fn initial_ctx
            >|= fun result ->
            match result with
            | None -> respond_with_text reqd `Not_found initial_ctx.response
            | Some r -> respond_with_text reqd r.response.status_code r.response
          in
          out |> ignore)
    |> ignore
  in
  request_handler


let make_router (routes : (Httpaf.Method.t * string * 'a server) list) app_state =
  let request_handler _ reqd =
    let open Lwt.Infix in
    read_request_body reqd
    >|= (fun body ->
          log_request (Reqd.request reqd) body ;
          let req = Reqd.request reqd in
          let initial_ctx =
            make_ctx app_state req.Request.meth req.target (Some body) req.headers
          in
          let found_handler =
            List.find routes ~f:(fun (meth, structure, _handler) ->
                let is_match, _updated_context =
                  Router.parse_route structure req.target initial_ctx
                in
                is_match
                && String.(
                     Httpaf.Method.to_string meth = (req.Request.meth |> Httpaf.Method.to_string)))
          in
          match found_handler with
          | None -> respond_with_text reqd `Not_found initial_ctx.response
          | Some (_, structure, handler) ->
              (* Todo: Fix this double call *)
              (* We are calling this a second time cos we need to the update context *)
              let _, updated_context = Router.parse_route structure req.target initial_ctx in
              let result' = updated_context |> handler in
              let out =
                result'
                >|= fun result ->
                match result with
                | None -> respond_with_text reqd `Not_found initial_ctx.response
                | Some r -> respond_with_text reqd r.response.status_code r.response
              in
              out |> ignore)
    |> ignore
  in
  request_handler


let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  ( match error with
  | `Exn exn ->
      Body.write_string response_body (Exn.to_string exn) ;
      Body.write_string response_body "\n"
  | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error) ) ;
  Body.close_writer response_body
