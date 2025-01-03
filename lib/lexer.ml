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

let rec read_identifier_aux (lexer : t) (acc : string) : t * string =
  match lexer.ch with
  | None -> (lexer, acc)
  | Some x ->
      if is_char x then
        read_identifier_aux (forward lexer) (acc ^ Char.escaped x)
      else (lexer, acc)

let read_identifier (lexer : t) : t * Token.t=
  let x = read_identifier_aux lexer "" in
  match snd x with
  | "let" -> (fst x, Token.Let)
  | "fn" -> (fst x, Token.Function)
  | "true" -> (fst x, Token.True)
  | "false" -> (fst x, Token.False)
  | "if" -> (fst x, Token.If)
  | "else" -> (fst x, Token.Else)
  | "return" -> (fst x, Token.Return)
  | y -> (fst x, (Token.Ident y))

let rec read_number_aux (lexer : t) (acc : string) : t * Token.t =
  match lexer.ch with
  | None -> (lexer, (Token.Int acc))
  | Some x ->
      if is_digit x then read_number_aux (forward lexer) (acc ^ Char.escaped x)
      else (lexer, (Token.Int acc))

let read_number (lexer : t) : t * Token.t = read_number_aux lexer ""

let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
      skip_whitespace (forward lexer)
  | _ -> lexer

let next_token (lexer : t) : t * Token.t =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (forward lexer, Token.Eof)
  | Some '=' ->
      if peek_char lexer = Some '=' then (forward (forward lexer), Token.Eq)
      else (forward lexer, Token.Assign)
  | Some '+' -> (forward lexer, Token.Plus)
  | Some '-' -> (forward lexer, Token.Minus)
  | Some '!' ->
      if peek_char lexer = Some '=' then
        (forward (forward lexer), Token.Not_eq)
      else (forward lexer, Token.Bang)
  | Some '*' -> (forward lexer, Token.Asterisk)
  | Some '/' -> (forward lexer, Token.Slash)
  | Some '<' -> (forward lexer, Token.LT)
  | Some '>' -> (forward lexer, Token.GT)
  | Some ',' -> (forward lexer, Token.Comma)
  | Some ';' -> (forward lexer, Token.Semicolon)
  | Some '(' -> (forward lexer, Token.LParen)
  | Some ')' -> (forward lexer, Token.RParen)
  | Some '{' -> (forward lexer, Token.LBrace)
  | Some '}' -> (forward lexer, Token.RBrace)
  | Some x ->
      if is_char x then read_identifier lexer
      else if is_digit x then read_number lexer
      else (forward lexer, Token.Illegal)

let rec show_aux (lexer : t) (acc : string) : string =
  let lexer, tok = next_token lexer in
  match tok with
  | Token.Eof -> acc
  | x -> show_aux lexer (acc ^ Token.to_string x)

let show (lexer : t) = show_aux lexer ""
