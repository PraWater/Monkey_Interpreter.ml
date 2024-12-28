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

let read_identifier (lexer : t) : t * Token.t option =
  let x = read_identifier_aux lexer "" in
  match snd x with
  | "let" -> (fst x, Some Token.Let)
  | "fn" -> (fst x, Some Token.Function)
  | "true" -> (fst x, Some Token.True)
  | "false" -> (fst x, Some Token.False)
  | "if" -> (fst x, Some Token.If)
  | "else" -> (fst x, Some Token.Else)
  | "return" -> (fst x, Some Token.Return)
  | y -> (fst x, Some (Token.Ident y))

let rec read_number_aux (lexer : t) (acc : string) : t * Token.t option =
  match lexer.ch with
  | None -> (lexer, Some (Token.Int acc))
  | Some x ->
      if is_digit x then read_number_aux (forward lexer) (acc ^ Char.escaped x)
      else (lexer, Some (Token.Int acc))

let read_number (lexer : t) : t * Token.t option = read_number_aux lexer ""

let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
      skip_whitespace (forward lexer)
  | _ -> lexer

let next_token (lexer : t) : t * Token.t option =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (forward lexer, Some Token.Eof)
  | Some '=' ->
      if peek_char lexer = Some '=' then (forward (forward lexer), Some Token.Eq)
      else (forward lexer, Some Token.Assign)
  | Some '+' -> (forward lexer, Some Token.Plus)
  | Some '-' -> (forward lexer, Some Token.Minus)
  | Some '!' ->
      if peek_char lexer = Some '=' then
        (forward (forward lexer), Some Token.Not_eq)
      else (forward lexer, Some Token.Bang)
  | Some '*' -> (forward lexer, Some Token.Asterisk)
  | Some '/' -> (forward lexer, Some Token.Slash)
  | Some '<' -> (forward lexer, Some Token.LT)
  | Some '>' -> (forward lexer, Some Token.GT)
  | Some ',' -> (forward lexer, Some Token.Comma)
  | Some ';' -> (forward lexer, Some Token.Semicolon)
  | Some '(' -> (forward lexer, Some Token.LParen)
  | Some ')' -> (forward lexer, Some Token.RParen)
  | Some '{' -> (forward lexer, Some Token.LBrace)
  | Some '}' -> (forward lexer, Some Token.RBrace)
  | Some x ->
      if is_char x then read_identifier lexer
      else if is_digit x then read_number lexer
      else (forward lexer, Some Token.Illegal)

let rec show_aux (lexer : t) (acc : string) : string =
  let lexer, tok = next_token lexer in
  match tok with
  | None -> acc
  | Some x ->
      match x with
      | Token.Eof -> acc
      | _ -> show_aux lexer (acc ^ Token.to_string x)

let show (lexer : t) = show_aux lexer ""
