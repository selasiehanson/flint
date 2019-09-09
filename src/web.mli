type http_request =
  { meth: Httpaf.Method.t
  ; path: string
  ; headers: (string * string) list
  ; body: string option
  ; path_params: (string * string) list
  ; query_params: (string * string) list }

type http_response = {status_code: Httpaf.Status.t; body: string; headers: (string * string) list}

val make_request :
  Httpaf.Method.t -> string -> (string * string) list -> string option -> http_request

val make_response : Httpaf.Status.t -> (string * string) list -> string -> http_response

type 'a http_context = {request: http_request; response: http_response; state: 'a; continue: bool}

type 'a web_server = 'a http_context -> 'a http_context option Lwt.t

val middleware_combine :
  'a web_server -> 'a web_server -> 'a http_context -> 'a http_context option Lwt.t

val bad_request : string -> 'a http_context -> 'a http_context option Lwt.t

val ok : string -> 'a http_context -> 'a http_context option Lwt.t

module Helpers : sig
  val as_json : 'a http_context -> 'a http_context Lwt.t
end

module Infix : sig
  val ( <|> ) : 'a web_server -> 'a web_server -> 'a http_context -> 'a http_context option Lwt.t
end

module Router : sig
  val pull_path_param : Core.String.t -> Core.String.t

  val parse_query_params : Core.String.t -> (Core.String.t * Core.String.t) Core.List.t

  val parse_route : string -> string -> 'a http_context -> bool * 'a http_context
end

val get_content_length : Core.String.t -> int

val read_request_body : Httpaf.Reqd.t -> string Lwt.t

val make_cust_router :
  (Httpaf.Method.t * string * 'a web_server) list -> 'a -> 'b -> Httpaf.Reqd.t -> unit

val error_handler : Unix.sockaddr -> Httpaf.Server_connection.error_handler
