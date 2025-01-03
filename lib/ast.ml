type statement =
  | Let of { name : expression; value : expression }
  | Return of expression
  | Expression of expression
  | Block of statement list
  | Nil

and expression =
  | Identifier of string
  | IntLiteral of int
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
  | Call of { fn : expression; arguments : expression list }

and program = { statements : statement list }

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
      let params = String.sub params 0 (String.length params - 2) in
      "fn(" ^ params ^ ")" ^ stm_to_string body
  | Call { fn; arguments } ->
      let args =
        List.fold_left (fun acc x -> acc ^ exp_to_string x ^ ", ") "(" arguments
      in
      let args = String.sub args 0 (String.length args - 2) in
      exp_to_string fn ^ args ^ ")"

and prog_to_string (prog : program) : string =
  List.fold_left (fun acc x -> acc ^ stm_to_string x) "" prog.statements
