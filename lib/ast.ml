type statement =
  | Let of { name : expression; value : expression }
  | Return of expression
  | Expression of expression
  | Block of statement list
  | Nil

and expression =
  | Identifier of string
  | IntLiteral of int
  | StringLiteral of string
  | ArrayLiteral of expression list
  | HashLiteral of { keys : expression list; values : expression list }
  | PrefixExpression of { operator : string; right : expression }
  | InfixExpression of {
      left : expression;
      operator : string;
      right : expression;
    }
  | BoolLiteral of bool
  | IfExpression of {
      condition : expression;
      consequence : statement;
      alternative : statement;
    }
  | FunctionLiteral of { parameters : expression list; body : statement }
  | MacroLiteral of { parameters : expression list; body : statement }
  | Call of { fn : expression; arguments : expression list }
  | IndexExpression of { left : expression; index : expression }
  | QuoteExpression of expression
  | UnquoteExpression of expression

and program = { statements : statement list }

let sub_if_longer_than (str : string) (length : int) : string =
  if String.length str >= length then
    String.sub str 0 (String.length str - length)
  else str

let rec stm_to_string (stm : statement) : string =
  match stm with
  | Let { name; value } ->
      "let " ^ exp_to_string name ^ " = " ^ exp_to_string value ^ ";\n"
  | Return exp -> "return " ^ exp_to_string exp ^ ";\n"
  | Expression exp -> exp_to_string exp ^ ";\n"
  | Block statements ->
      "{\n"
      ^ List.fold_left (fun acc x -> acc ^ stm_to_string x) "" statements
      ^ "}"
  | Nil -> "Nil\n"

and exp_to_string (exp : expression) : string =
  match exp with
  | Identifier name -> name
  | IntLiteral value -> string_of_int value
  | StringLiteral value -> value
  | ArrayLiteral exps ->
      let exps =
        List.fold_left (fun acc x -> acc ^ exp_to_string x ^ ", ") "" exps
      in
      let exps = sub_if_longer_than exps 2 in
      "[" ^ exps ^ "]"
  | HashLiteral { keys; values } ->
      let rec r_hash_string (keys : expression list) (values : expression list)
          (acc : string) : string =
        match (keys, values) with
        | [], [] -> acc
        | h1 :: [], h2 :: [] ->
            acc ^ exp_to_string h1 ^ " : " ^ exp_to_string h2
        | h1 :: t1, h2 :: t2 ->
            r_hash_string t1 t2
              (acc ^ exp_to_string h1 ^ " : " ^ exp_to_string h2 ^ ", ")
        | _, _ -> failwith "hash to string error"
      in
      "{ " ^ r_hash_string keys values "" ^ " }"
  | PrefixExpression { operator; right } ->
      "(" ^ operator ^ exp_to_string right ^ ")"
  | InfixExpression { left; operator; right } ->
      "(" ^ exp_to_string left ^ operator ^ exp_to_string right ^ ")"
  | BoolLiteral value -> if value then "true" else "false"
  | IfExpression { condition; consequence; alternative } ->
      "if " ^ exp_to_string condition ^ " " ^ stm_to_string consequence
      ^ if alternative != Nil then " else " ^ stm_to_string alternative else ""
  | FunctionLiteral { parameters; body } ->
      let params =
        List.fold_left (fun acc x -> acc ^ exp_to_string x ^ ", ") "" parameters
      in
      let params = sub_if_longer_than params 2 in
      "fn(" ^ params ^ ")" ^ stm_to_string body
  | MacroLiteral { parameters; body } ->
      let params =
        List.fold_left (fun acc x -> acc ^ exp_to_string x ^ ", ") "" parameters
      in
      let params = sub_if_longer_than params 2 in
      "macro(" ^ params ^ ")" ^ stm_to_string body
  | Call { fn; arguments } ->
      let args =
        List.fold_left (fun acc x -> acc ^ exp_to_string x ^ ", ") "(" arguments
      in
      let args = sub_if_longer_than args 2 in
      exp_to_string fn ^ args ^ ")"
  | IndexExpression { left; index } ->
      "(" ^ exp_to_string left ^ "[" ^ exp_to_string index ^ "])"
  | QuoteExpression exp -> "Quote(" ^ exp_to_string exp ^ ")"
  | UnquoteExpression exp -> "Unquote(" ^ exp_to_string exp ^ ")"

and prog_to_string (prog : program) : string =
  List.fold_left (fun acc x -> acc ^ stm_to_string x) "" prog.statements

and modify_expression (modify_func : expression -> expression)
    (exp : expression) : expression =
  let exp =
    match exp with
    | ArrayLiteral exps ->
        ArrayLiteral (List.map (modify_expression modify_func) exps)
    | HashLiteral { keys; values } ->
        HashLiteral
          {
            keys = List.map (modify_expression modify_func) keys;
            values = List.map (modify_expression modify_func) values;
          }
    | PrefixExpression { operator; right } ->
        PrefixExpression
          { operator; right = modify_expression modify_func right }
    | InfixExpression { left; operator; right } ->
        InfixExpression
          {
            left = modify_expression modify_func left;
            operator;
            right = modify_expression modify_func right;
          }
    | IfExpression { condition; consequence; alternative } ->
        IfExpression
          {
            condition = modify_expression modify_func condition;
            consequence = modify_statement modify_func consequence;
            alternative = modify_statement modify_func alternative;
          }
    | IndexExpression { left; index } ->
        IndexExpression
          {
            left = modify_expression modify_func left;
            index = modify_expression modify_func index;
          }
    | Call { fn; arguments } ->
        Call
          {
            fn = modify_expression modify_func fn;
            arguments = List.map (modify_expression modify_func) arguments;
          }
    | FunctionLiteral { parameters; body } ->
        FunctionLiteral
          {
            parameters = List.map (modify_expression modify_func) parameters;
            body = modify_statement modify_func body;
          }
    | QuoteExpression exp -> QuoteExpression (modify_expression modify_func exp)
    | UnquoteExpression exp ->
        UnquoteExpression (modify_expression modify_func exp)
    | _ -> exp
  in
  modify_func exp

and modify_statement (modify_func : expression -> expression) (stm : statement)
    : statement =
  match stm with
  | Let { name; value } ->
      Let
        {
          name = modify_expression modify_func name;
          value = modify_expression modify_func value;
        }
  | Return exp -> Return (modify_expression modify_func exp)
  | Expression exp -> Expression (modify_expression modify_func exp)
  | Block stms -> Block (List.map (modify_statement modify_func) stms)
  | Nil -> Nil

and modify_program (modify_func : expression -> expression) (prog : program) :
    program =
  let statements =
    List.map
      (function stm -> modify_statement modify_func stm)
      prog.statements
  in
  { statements }
