type t = { input : string; position : int; ch : char option }

let init (input : string) : t =
  if String.length input = 0 then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let forward (lexer : t) : t =
  if String.length lexer.input - 1 <= lexer.position then
    { lexer with position = lexer.position + 1; ch = None }
  else
    {
      lexer with
      position = lexer.position + 1;
      ch = Some (String.get lexer.input (lexer.position + 1));
    }

let peek_char (lexer : t) : char option =
  if String.length lexer.input - 1 <= lexer.position then None
  else Some (String.get lexer.input (lexer.position + 1))

let is_char (ch : char) : bool =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || Char.equal ch '_'

let is_digit (ch : char) : bool = ch >= '0' && ch <= '9'

let read_identifier (lexer : t) : t * Token.t =
  let rec read_identifier_aux (lexer : t) (acc : string) : t * string =
    match lexer.ch with
    | None -> (lexer, acc)
    | Some x ->
        if is_char x then
          read_identifier_aux (forward lexer) (acc ^ Char.escaped x)
        else (lexer, acc)
  in
  let lexer, value = read_identifier_aux lexer "" in
  match value with
  | "let" -> (lexer, Token.Let)
  | "fn" -> (lexer, Token.Function)
  | "true" -> (lexer, Token.True)
  | "false" -> (lexer, Token.False)
  | "if" -> (lexer, Token.If)
  | "else" -> (lexer, Token.Else)
  | "return" -> (lexer, Token.Return)
  | "macro" -> (lexer, Token.Macro)
  | y -> (lexer, Token.Ident y)

let read_number (lexer : t) : t * Token.t =
  let rec read_number_aux (lexer : t) (acc : string) : t * Token.t =
    match lexer.ch with
    | None -> (lexer, Token.Int acc)
    | Some x ->
        if is_digit x then read_number_aux (forward lexer) (acc ^ Char.escaped x)
        else (lexer, Token.Int acc)
  in
  read_number_aux lexer ""

let read_string (lexer : t) : t * Token.t =
  let rec read_string_aux (lexer : t) (acc : string) : t * Token.t =
    match lexer.ch with
    | None | Some '"' -> (forward lexer, Token.String acc)
    | Some x -> read_string_aux (forward lexer) (acc ^ Char.escaped x)
  in
  let lexer = forward lexer in
  read_string_aux lexer ""

let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
      skip_whitespace (forward lexer)
  | _ -> lexer

let rec skip_past (lexer : t) (ch : char) : t =
  match lexer.ch with
  | None -> lexer
  | Some x when x = ch -> forward lexer
  | Some _ -> skip_past (forward lexer) ch

let rec next_token (lexer : t) : t * Token.t =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (forward lexer, Token.Eof)
  | Some '=' ->
      if peek_char lexer = Some '=' then (forward (forward lexer), Token.Eq)
      else (forward lexer, Token.Assign)
  | Some '+' -> (forward lexer, Token.Plus)
  | Some '-' -> (forward lexer, Token.Minus)
  | Some '!' ->
      if peek_char lexer = Some '=' then (forward (forward lexer), Token.Not_eq)
      else (forward lexer, Token.Bang)
  | Some '*' -> (forward lexer, Token.Asterisk)
  | Some '/' -> (forward lexer, Token.Slash)
  | Some '<' -> (forward lexer, Token.LT)
  | Some '>' -> (forward lexer, Token.GT)
  | Some ',' -> (forward lexer, Token.Comma)
  | Some ';' -> (forward lexer, Token.Semicolon)
  | Some ':' -> (forward lexer, Token.Colon)
  | Some '(' -> (forward lexer, Token.LParen)
  | Some ')' -> (forward lexer, Token.RParen)
  | Some '{' -> (forward lexer, Token.LBrace)
  | Some '}' -> (forward lexer, Token.RBrace)
  | Some '[' -> (forward lexer, Token.LBracket)
  | Some ']' -> (forward lexer, Token.RBracket)
  | Some '"' -> read_string lexer
  | Some '#' -> next_token (skip_past lexer '\n')
  | Some x ->
      if is_char x then read_identifier lexer
      else if is_digit x then read_number lexer
      else (forward lexer, Token.Illegal)

let show (lexer : t) =
  let rec show_aux (lexer : t) (acc : string) : string =
    let lexer, tok = next_token lexer in
    match tok with
    | Token.Eof -> acc
    | x -> show_aux lexer (acc ^ Token.to_string x)
  in
  show_aux lexer ""
