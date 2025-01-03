type t

val init : Lexer.t -> t
val parse_program : t -> t * Ast.program
val show : t -> string
