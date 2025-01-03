type t

val init : string -> t
val next_token : t -> t * Token.t
val show : t -> string
