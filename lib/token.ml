type t =
  | Illegal
  | Eof
  (*Identifiers + Literals*)
  | Ident of string
  | Int of string
  (*Operators*)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Eq
  | Not_eq
  (*Delimiters*)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (*Keywords*)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return

let to_string (token : t) =
  match token with
  | Illegal -> "Illegal\n"
  | Ident x -> "Ident " ^ x ^ "\n"
  | Int x -> "Int " ^ x ^ "\n"
  | Assign -> "Assign\n"
  | Plus -> "Plus\n"
  | Minus -> "Minus\n"
  | Bang -> "Bang\n"
  | Asterisk -> "Asterisk\n"
  | Slash -> "Slash\n"
  | LT -> "LT\n"
  | GT -> "GT\n"
  | Eq -> "Eq\n"
  | Not_eq -> "Not_eq\n"
  | Comma -> "Comma\n"
  | Semicolon -> "Semicolon\n"
  | LParen -> "LParen\n"
  | RParen -> "RParen\n"
  | LBrace -> "LBrace\n"
  | RBrace -> "RBrace\n"
  | Function -> "Function\n"
  | Let -> "Let\n"
  | True -> "True\n"
  | False -> "False\n"
  | If -> "If\n"
  | Else -> "Else\n"
  | Return -> "Return\n"
  | Eof -> "Eof\n"

let to_value (token : t) =
  match token with
  | Illegal -> "Illegal\n"
  | Ident x -> x
  | Int x -> x
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | LT -> "<"
  | GT -> ">"
  | Eq -> "=="
  | Not_eq -> "!="
  | Comma -> ","
  | Semicolon -> ";"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | Function -> "func"
  | Let -> "let"
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
  | Eof -> "Eof\n"
