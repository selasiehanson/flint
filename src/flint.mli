module Web = Web

type config = Runner.config

val program : config -> (Httpaf.Method.t * string * 'a Web.server) list -> 'a -> 'b Lwt.t

val simple_program : config -> (Httpaf.Method.t * string list -> 'a Web.server) -> 'a -> 'c Lwt.t
