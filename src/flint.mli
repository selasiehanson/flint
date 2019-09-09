module Web = Web

val program : int -> (Httpaf.Method.t * string * 'a Web.server) list -> 'a -> 'b Lwt.t
