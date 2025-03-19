type t =
  | Illegal
  | Eof
  (*Identifiers + Literals*)
  | Ident of string
  | Int of string
  | String of string
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
  | Colon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  (*Keywords*)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  | Macro

let to_string (token : t) =
  match token with
  | Illegal -> "Illegal\n"
  | Ident x -> "Ident " ^ x ^ "\n"
  | Int x -> "Int " ^ x ^ "\n"
  | String x -> "String " ^ x ^ "\n"
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
  | Colon -> "Colon\n"
  | LParen -> "LParen\n"
  | RParen -> "RParen\n"
  | LBrace -> "LBrace\n"
  | RBrace -> "RBrace\n"
  | LBracket -> "LBracket\n"
  | RBracket -> "RBracket\n"
  | Function -> "Function\n"
  | Let -> "Let\n"
  | True -> "True\n"
  | False -> "False\n"
  | If -> "If\n"
  | Else -> "Else\n"
  | Return -> "Return\n"
  | Macro -> "Macro\n"
  | Eof -> "Eof\n"

let to_value (token : t) =
  match token with
  | Illegal -> "Illegal\n"
  | Ident x -> x
  | Int x -> x
  | String x -> x
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
  | Colon -> ":"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Function -> "func"
  | Let -> "let"
  | True -> "true"
  | False -> "false"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
  | Macro -> "macro"
  | Eof -> "Eof\n"
