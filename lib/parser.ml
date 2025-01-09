type t = { lexer : Lexer.t; curr_token : Token.t; peek_token : Token.t }

let init (lexer : Lexer.t) : t =
  let lexer, curr_token = Lexer.next_token lexer in
  let lexer, peek_token = Lexer.next_token lexer in
  { lexer; curr_token; peek_token }

let advance (parser : t) : t =
  let curr_token = parser.peek_token in
  let lexer, peek_token = Lexer.next_token parser.lexer in
  { lexer; curr_token; peek_token }

let check_advance (parser : t) (token : Token.t) : t =
  if parser.curr_token = token then advance parser
  else failwith ("Token " ^ Token.to_string token ^ " not found.")

let peek_advance (parser : t) (token : Token.t) : t =
  if parser.peek_token = token then advance parser
  else failwith ("Token " ^ Token.to_string token ^ " not found.")

let parse_identifier (parser : t) : Ast.expression =
  match parser.curr_token with
  | Token.Ident name -> Ast.Identifier name
  | _ ->
      failwith
        ("Identifier not found, instead found "
        ^ Token.to_value parser.curr_token)

let prefix_precedence = 7
let least_precedence = 1

let get_precedence (token : Token.t) : int =
  match token with
  | Token.Eq | Token.Not_eq -> 2
  | Token.LT | Token.GT -> 3
  | Token.Plus | Token.Minus -> 4
  | Token.Slash | Token.Asterisk -> 5
  | Token.LParen -> 6
  | Token.LBrace -> 8
  | Token.LBracket -> 9
  | _ -> 1

let rec parse_expression (parser : t) (precedence : int) : t * Ast.expression =
  let parser, exp = parse_prefix parser in
  r_parse_infix parser exp precedence

and parse_prefix (parser : t) : t * Ast.expression =
  match parser.curr_token with
  | Token.Ident name -> (parser, Ast.Identifier name)
  | Token.Int value -> (parser, Ast.IntLiteral (int_of_string value))
  | Token.Bang | Token.Minus -> parse_prefix_expression parser
  | Token.True -> (parser, Ast.BoolLiteral true)
  | Token.False -> (parser, Ast.BoolLiteral false)
  | Token.LParen -> parse_grouped_expression parser
  | Token.LBracket -> parse_array_literal parser
  | Token.LBrace -> parse_hash_literal parser
  | Token.If -> parse_if_expression parser
  | Token.Function -> parse_function_literal parser
  | Token.String value -> (parser, Ast.StringLiteral value)
  | _ ->
      failwith ("Not implemented for token " ^ Token.to_value parser.curr_token)

and parse_prefix_expression (parser : t) : t * Ast.expression =
  let operator = if parser.curr_token = Token.Bang then "!" else "-" in
  let parser = advance parser in
  let parser, right = parse_expression parser prefix_precedence in
  (parser, Ast.PrefixExpression { operator; right })

and parse_grouped_expression (parser : t) : t * Ast.expression =
  let parser = advance parser in
  let parser, exp = parse_expression parser least_precedence in
  let parser = peek_advance parser Token.RParen in
  (parser, exp)

and parse_array_literal (parser : t) : t * Ast.expression =
  let parser = advance parser in
  let parser, exps = parse_expression_list parser [] Token.RBracket in
  (parser, Ast.ArrayLiteral exps)

and parse_hash_literal (parser : t) : t * Ast.expression =
  let rec r_parse_hash_literal (parser : t) (keys : Ast.expression list)
      (values : Ast.expression list) :
      t * Ast.expression list * Ast.expression list =
    if parser.curr_token = Token.RBrace || parser.curr_token = Token.Eof then
      (parser, List.rev keys, List.rev values)
    else
      let parser, key = parse_expression parser least_precedence in
      let parser = peek_advance parser Token.Colon in
      let parser = advance parser in
      let parser, value = parse_expression parser least_precedence in
      let parser = advance parser in
      let parser =
        if parser.curr_token = Token.Comma then advance parser else parser
      in
      r_parse_hash_literal parser (key :: keys) (value :: values)
  in
  let parser = advance parser in
  let parser, keys, values = r_parse_hash_literal parser [] [] in
  (parser, Ast.HashLiteral { keys; values })

and parse_expression_list (parser : t) (acc : Ast.expression list)
    (tok : Token.t) : t * Ast.expression list =
  if parser.curr_token = tok || parser.curr_token = Token.Eof then (parser, acc)
  else
    let parser, expression = parse_expression parser least_precedence in
    let parser =
      if parser.peek_token = Token.Comma then advance parser else parser
    in
    parse_expression_list (advance parser) (acc @ [ expression ]) tok

and parse_if_expression (parser : t) : t * Ast.expression =
  let parser = peek_advance parser Token.LParen in
  let parser = advance parser in
  let parser, condition = parse_expression parser least_precedence in
  let parser = peek_advance parser Token.RParen in
  let parser = peek_advance parser Token.LBrace in
  let parser, consequence = parse_block_statement parser in
  let parser, alternative =
    if parser.peek_token = Token.Else then
      let parser = advance parser in
      let parser = peek_advance parser Token.LBrace in
      parse_block_statement parser
    else (parser, Ast.Nil)
  in
  (parser, Ast.IfExpression { condition; consequence; alternative })

and parse_operator (token : Token.t) : string = Token.to_value token

and r_parse_infix (parser : t) (exp : Ast.expression) (precedence : int) :
    t * Ast.expression =
  if
    parser.peek_token = Token.Semicolon
    || parser.peek_token = Token.Eof
    || precedence >= get_precedence parser.peek_token
  then (parser, exp)
  else
    let parser = advance parser in
    let parser, exp = parse_infix parser exp in
    r_parse_infix parser exp precedence

and parse_infix (parser : t) (exp : Ast.expression) : t * Ast.expression =
  match parser.curr_token with
  | Token.LParen -> parse_call_expression parser exp
  | Token.LBracket -> parse_index_expression parser exp
  | _ -> parse_infix_expression parser exp

and parse_infix_expression (parser : t) (left : Ast.expression) :
    t * Ast.expression =
  let precedence = get_precedence parser.curr_token in
  let operator = parse_operator parser.curr_token in
  let parser = advance parser in
  let parser, right = parse_expression parser precedence in
  (parser, Ast.InfixExpression { left; operator; right })

and parse_parameters (parser : t) : t * Ast.expression list =
  let parser = advance parser in
  let rec r_parse_parameters (parser : t) (acc : Ast.expression list) :
      t * Ast.expression list =
    if parser.curr_token = Token.RParen || parser.curr_token = Token.Eof then
      (parser, acc)
    else
      let identifier = parse_identifier parser in
      let parser =
        if parser.peek_token = Token.Comma then advance parser else parser
      in
      r_parse_parameters (advance parser) (acc @ [ identifier ])
  in
  r_parse_parameters parser []

and parse_function_literal (parser : t) : t * Ast.expression =
  let parser = peek_advance parser Token.LParen in
  let parser, parameters = parse_parameters parser in
  let parser = peek_advance parser Token.LBrace in
  let parser, body = parse_block_statement parser in
  (parser, Ast.FunctionLiteral { parameters; body })

and parse_arguments (parser : t) : t * Ast.expression list =
  let parser = advance parser in
  parse_expression_list parser [] Token.RParen

and parse_call_expression (parser : t) (fn : Ast.expression) :
    t * Ast.expression =
  let parser, arguments = parse_arguments parser in
  (parser, Ast.Call { fn; arguments })

and parse_index_expression (parser : t) (left : Ast.expression) :
    t * Ast.expression =
  let parser = advance parser in
  let parser, index = parse_expression parser least_precedence in
  let parser = peek_advance parser Token.RBracket in
  (parser, Ast.IndexExpression { left; index })

and parse_let_statement (parser : t) : t * Ast.statement =
  let parser = advance parser in
  let name = parse_identifier parser in
  let parser = advance parser in
  let parser = check_advance parser Token.Assign in
  let parser, value = parse_expression parser least_precedence in
  let parser = advance parser in
  let parser = check_advance parser Token.Semicolon in
  (parser, Ast.Let { name; value })

and parse_return_statement (parser : t) : t * Ast.statement =
  let parser = advance parser in
  let parser, value = parse_expression parser least_precedence in
  let parser = advance parser in
  let parser = check_advance parser Token.Semicolon in
  (parser, Ast.Return value)

and parse_expression_statement (parser : t) : t * Ast.statement =
  let parser, value = parse_expression parser least_precedence in
  let parser = advance parser in
  let parser =
    if parser.curr_token = Token.Semicolon then advance parser else parser
  in
  (parser, Ast.Expression value)

and parse_block_statement (parser : t) : t * Ast.statement =
  let parser = advance parser in
  let rec r_parse_block_statement (parser : t) (acc : Ast.statement list) :
      t * Ast.statement list =
    if parser.curr_token = Token.RBrace || parser.curr_token = Token.Eof then
      (parser, acc)
    else
      let parser, statement = parse_statement parser in
      let acc = if statement = Ast.Nil then acc else acc @ [ statement ] in
      r_parse_block_statement parser acc
  in
  let parser, statements = r_parse_block_statement parser [] in
  (parser, Ast.Block statements)

and parse_statement (parser : t) : t * Ast.statement =
  match parser.curr_token with
  | Token.Eof -> (parser, Ast.Nil)
  | Token.Let -> parse_let_statement parser
  | Token.Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser

let parse_program (parser : t) : t * Ast.program =
  let rec parse_program_aux (parser : t) (program : Ast.program) :
      t * Ast.program =
    let parser, statement = parse_statement parser in
    if statement = Ast.Nil then (parser, program)
    else
      let statements = program.statements @ [ statement ] in
      parse_program_aux parser Ast.{ statements }
  in
  let statements = [] in
  parse_program_aux parser Ast.{ statements }

let show (parser : t) : string =
  let _, prog = parse_program parser in
  Ast.prog_to_string prog
