let true_object = Object.Boolean true
let false_object = Object.Boolean false
let null_object = Object.Null

let rec eval (prog : Ast.program) (env : Env.t) : Object.t * Env.t =
  let rec r_eval (statements : Ast.statement list) (env : Env.t) :
      Object.t * Env.t =
    match statements with
    | [] -> (null_object, env)
    | h :: [] ->
        let result, env = eval_statement h env in
        (unwrap_return_value result, env)
    | h :: t -> (
        let result, env = eval_statement h env in
        match result with
        | Object.ReturnValue value -> (value, env)
        | _ -> r_eval t env)
  in
  r_eval prog.statements env

and eval_statement (stm : Ast.statement) (env : Env.t) : Object.t * Env.t =
  match stm with
  | Ast.Expression exp -> eval_expression exp env
  | Ast.Block statements -> eval_block_statement statements env
  | Ast.Let { name; value } -> eval_let_statement name value env
  | Ast.Return exp -> eval_return_statement exp env
  | Ast.Nil -> (null_object, env)

and eval_let_statement (name : Ast.expression) (value : Ast.expression)
    (env : Env.t) : Object.t * Env.t =
  let name = Ast.exp_to_string name in
  let value, env = eval_expression value env in
  let env = Env.set name value env in
  (null_object, env)

and eval_block_statement (statements : Ast.statement list) (env : Env.t) :
    Object.t * Env.t =
  let rec r_eval_block (statements : Ast.statement list) (env : Env.t) :
      Object.t * Env.t =
    match statements with
    | [] -> (null_object, env)
    | h :: [] -> eval_statement h env
    | h :: t -> (
        let result, env = eval_statement h env in
        match result with
        | Object.ReturnValue _ -> (result, env)
        | _ -> r_eval_block t env)
  in
  r_eval_block statements env

and eval_return_statement (exp : Ast.expression) (env : Env.t) :
    Object.t * Env.t =
  let result, env = eval_expression exp env in
  (Object.ReturnValue result, env)

and eval_expression (exp : Ast.expression) (env : Env.t) : Object.t * Env.t =
  match exp with
  | Ast.IntLiteral value -> (Object.Integer value, env)
  | Ast.BoolLiteral value -> (bool_to_boolean_object value, env)
  | Ast.PrefixExpression { operator = "!"; right } ->
      eval_bang_expression right env
  | Ast.PrefixExpression { operator = "-"; right } ->
      eval_minus_expression right env
  | Ast.InfixExpression { left; operator; right } ->
      eval_infix_expression operator left right env
  | Ast.IfExpression { condition; consequence; alternative } ->
      eval_if_expression condition consequence alternative env
  | Ast.Identifier name -> (Env.get name env, env)
  | Ast.FunctionLiteral { parameters; body } ->
      (Object.Function { parameters; body }, env)
  | Ast.Call { fn; arguments } -> apply_function fn arguments env
  | _ -> failwith ("Couldn't eval expression: " ^ Ast.exp_to_string exp)

and bool_to_boolean_object (value : bool) : Object.t =
  if value then true_object else false_object

and eval_bang_expression (right : Ast.expression) (env : Env.t) :
    Object.t * Env.t =
  let right, env = eval_expression right env in
  if right = true_object then (false_object, env)
  else if right = false_object then (true_object, env)
  else if right = null_object then (true_object, env)
  else failwith ("Unexpected for bang operator: " ^ Object.inspect right)

and eval_minus_expression (right : Ast.expression) (env : Env.t) :
    Object.t * Env.t =
  let right, env = eval_expression right env in
  match right with
  | Object.Integer value -> (Object.Integer (-value), env)
  | _ -> failwith ("Unexpected for minux operator: " ^ Object.inspect right)

and eval_infix_expression (operator : string) (left : Ast.expression)
    (right : Ast.expression) (env : Env.t) : Object.t * Env.t =
  let left, env = eval_expression left env in
  let right, env = eval_expression right env in
  let result =
    match (operator, left, right) with
    | "+", Object.Integer x, Object.Integer y -> Object.Integer (x + y)
    | "-", Object.Integer x, Object.Integer y -> Object.Integer (x - y)
    | "*", Object.Integer x, Object.Integer y -> Object.Integer (x * y)
    | "/", Object.Integer x, Object.Integer y -> Object.Integer (x / y)
    | "<", Object.Integer x, Object.Integer y -> bool_to_boolean_object (x < y)
    | ">", Object.Integer x, Object.Integer y -> bool_to_boolean_object (x > y)
    | "==", _, _ -> bool_to_boolean_object (left = right)
    | "!=", _, _ -> bool_to_boolean_object (left != right)
    | _ -> failwith "infix"
  in
  (result, env)

and eval_if_expression (condition : Ast.expression)
    (consequence : Ast.statement) (alternative : Ast.statement) (env : Env.t) :
    Object.t * Env.t =
  let condition, env = eval_expression condition env in
  if condition != null_object && condition != false_object then
    eval_statement consequence env
  else eval_statement alternative env

and apply_function (fn : Ast.expression) (arguments : Ast.expression list)
    (env : Env.t) : Object.t * Env.t =
  let fn_obj, env = eval_expression fn env in
  let body, parameters =
    match fn_obj with
    | Object.Function { body; parameters } -> (body, parameters)
    | _ -> failwith (Ast.exp_to_string fn ^ " not a function")
  in
  let extended_env = extend_function_env parameters arguments env in
  let evaluated = fst (eval_statement body extended_env) in
  (unwrap_return_value evaluated, env)

and extend_function_env (parameters : Ast.expression list)
    (arguments : Ast.expression list) (env : Env.t) : Env.t =
  let extended_env = Env.new_enclosed_env env in
  let rec r_extend (parameters : Ast.expression list)
      (arguments : Ast.expression list) (env : Env.t) : Env.t =
    match (parameters, arguments) with
    | [], [] -> env
    | h1 :: t1, h2 :: t2 ->
        let parameter =
          match h1 with
          | Ast.Identifier name -> name
          | _ -> failwith (Ast.exp_to_string h1 ^ " is not Identifier")
        in
        let argument, env = eval_expression h2 env in
        let env = Env.set parameter argument env in
        r_extend t1 t2 env
    | _, _ ->
        failwith "Number of parameters and number of arguments are not equal"
  in
  r_extend parameters arguments extended_env

and unwrap_return_value (obj : Object.t) : Object.t =
  match obj with Object.ReturnValue value -> value | _ -> obj
