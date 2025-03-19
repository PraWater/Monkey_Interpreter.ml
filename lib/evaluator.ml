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
  | Ast.StringLiteral value -> (Object.String value, env)
  | Ast.ArrayLiteral exps -> (eval_array_literal exps env, env)
  | Ast.HashLiteral { keys; values } -> (eval_hash_literal keys values env, env)
  | Ast.PrefixExpression { operator = "!"; right } ->
      eval_bang_expression right env
  | Ast.PrefixExpression { operator = "-"; right } ->
      eval_minus_expression right env
  | Ast.InfixExpression { left; operator; right } ->
      eval_infix_expression operator left right env
  | Ast.IfExpression { condition; consequence; alternative } ->
      eval_if_expression condition consequence alternative env
  | Ast.Identifier name -> (eval_identifier name env, env)
  | Ast.FunctionLiteral { parameters; body } ->
      (Object.Function { parameters; body }, env)
  | Ast.Call { fn; arguments } -> apply_function fn arguments env
  | Ast.QuoteExpression exp ->
      let modify_func (env : Env.t) (exp : Ast.expression) : Ast.expression =
        match exp with
        | Ast.UnquoteExpression exp ->
            let obj, _ = eval_expression exp env in
            Object.object_to_ast_expression obj
        | _ -> exp
      in
      let exp = Ast.modify_expression (modify_func env) exp in
      (Object.Quote exp, env)
  | Ast.IndexExpression { left; index } ->
      (eval_index_expression left index env, env)
  | _ -> failwith ("Couldn't eval expression: " ^ Ast.exp_to_string exp)

and bool_to_boolean_object (value : bool) : Object.t =
  if value then true_object else false_object

and eval_array_literal (exps : Ast.expression list) (env : Env.t) : Object.t =
  let elems = List.map (function exp -> fst (eval_expression exp env)) exps in
  Object.Array elems

and eval_hash_literal (keys : Ast.expression list)
    (values : Ast.expression list) (env : Env.t) : Object.t =
  let rec r_eval_hash_literal (keys : Object.t list) (values : Object.t list)
      (hash : Object.t) : Object.t =
    match (keys, values, hash) with
    | [], [], Object.Hash _ -> hash
    | h1 :: t1, h2 :: t2, Object.Hash h ->
        let hash =
          match h1 with
          | Object.String key ->
              Object.Hash
                { h with stringMap = Object.StringMap.add key h2 h.stringMap }
          | Object.Integer key ->
              Object.Hash { h with intMap = Object.IntMap.add key h2 h.intMap }
          | Object.Boolean true -> Object.Hash { h with trueItem = h2 }
          | Object.Boolean false -> Object.Hash { h with falseItem = h2 }
          | _ -> failwith "Unexpected type for key in hash"
        in
        r_eval_hash_literal t1 t2 hash
    | _ -> failwith "Hash literal error"
  in
  let keys = List.map (function exp -> fst (eval_expression exp env)) keys in
  let values =
    List.map (function exp -> fst (eval_expression exp env)) values
  in
  let hash =
    Object.Hash
      {
        stringMap = Object.StringMap.empty;
        intMap = Object.IntMap.empty;
        trueItem = null_object;
        falseItem = null_object;
        keys;
        values;
      }
  in
  r_eval_hash_literal keys values hash

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
  | _ -> failwith ("Unexpected for minus operator: " ^ Object.inspect right)

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
    | "+", Object.String x, Object.String y -> Object.String (x ^ y)
    | _ ->
        failwith
          ("Infix not defined for: " ^ operator ^ Object.inspect left
         ^ Object.inspect right)
  in
  (result, env)

and eval_if_expression (condition : Ast.expression)
    (consequence : Ast.statement) (alternative : Ast.statement) (env : Env.t) :
    Object.t * Env.t =
  let condition, env = eval_expression condition env in
  if condition != null_object && condition != false_object then
    eval_statement consequence env
  else eval_statement alternative env

and eval_identifier (name : string) (env : Env.t) : Object.t =
  match (Env.get name env, Object.get_builtin name) with
  | Some obj, _ -> obj
  | _, Some builtin -> builtin
  | None, None -> failwith (name ^ " not found in env and builtins")

and apply_function (fn : Ast.expression) (arguments : Ast.expression list)
    (env : Env.t) : Object.t * Env.t =
  let fn_obj, env = eval_expression fn env in
  match fn_obj with
  | Object.Function { body; parameters } ->
      let extended_env = extend_function_env parameters arguments env in
      let evaluated = fst (eval_statement body extended_env) in
      (unwrap_return_value evaluated, env)
  | Object.Builtin blt -> (
      match blt with
      | Object.Len -> apply_len arguments env
      | Object.First -> apply_first arguments env
      | Object.Last -> apply_last arguments env
      | Object.Rest -> apply_rest arguments env
      | Object.Push -> apply_push arguments env
      | Object.Puts -> apply_puts arguments env)
  | _ -> failwith (Ast.exp_to_string fn ^ " not a function")

and extend_function_env (parameters : Ast.expression list)
    (arguments : Ast.expression list) (env : Env.t) : Env.t =
  let extended_env = Env.new_enclosed_env env in
  let rec r_extend (parameters : Ast.expression list)
      (arguments : Ast.expression list) (extended_env : Env.t) : Env.t =
    match (parameters, arguments) with
    | [], [] -> extended_env
    | h1 :: t1, h2 :: t2 ->
        let parameter =
          match h1 with
          | Ast.Identifier name -> name
          | _ -> failwith (Ast.exp_to_string h1 ^ " is not Identifier")
        in
        let argument, _ = eval_expression h2 env in
        let extended_env = Env.set parameter argument extended_env in
        r_extend t1 t2 extended_env
    | _, _ ->
        failwith "Number of parameters and number of arguments are not equal"
  in
  r_extend parameters arguments extended_env

and unwrap_return_value (obj : Object.t) : Object.t =
  match obj with Object.ReturnValue value -> value | _ -> obj

and apply_len (arguments : Ast.expression list) (env : Env.t) : Object.t * Env.t
    =
  if List.length arguments = 1 then
    let arg = List.nth arguments 0 in
    let arg, env = eval_expression arg env in
    match arg with
    | Object.String str -> (Object.Integer (String.length str), env)
    | Object.Array arr -> (Object.Integer (List.length arr), env)
    | _ -> failwith "Not a string/array"
  else
    failwith
      ("Wrong number of arguments for len: "
      ^ string_of_int (List.length arguments))

and apply_first (arguments : Ast.expression list) (env : Env.t) :
    Object.t * Env.t =
  if List.length arguments = 1 then
    let arg = List.nth arguments 0 in
    (eval_index_expression arg (Ast.IntLiteral 0) env, env)
  else
    failwith
      ("Wrong number of arguments for first: "
      ^ string_of_int (List.length arguments))

and apply_last (arguments : Ast.expression list) (env : Env.t) :
    Object.t * Env.t =
  if List.length arguments = 1 then
    let arg = List.nth arguments 0 in
    let arg_obj, _ = eval_expression arg env in
    match arg_obj with
    | Object.Array arr -> (List.nth arr (List.length arr - 1), env)
    | _ -> failwith ("Not an array: " ^ Ast.exp_to_string arg)
  else
    failwith
      ("Wrong number of arguments for last: "
      ^ string_of_int (List.length arguments))

and apply_rest (arguments : Ast.expression list) (env : Env.t) :
    Object.t * Env.t =
  if List.length arguments = 1 then
    let arg = List.nth arguments 0 in
    let arg_obj, _ = eval_expression arg env in
    match arg_obj with
    | Object.Array arr -> (Object.Array (List.tl arr), env)
    | _ -> failwith ("Not an array: " ^ Ast.exp_to_string arg)
  else
    failwith
      ("Wrong number of arguments for rest: "
      ^ string_of_int (List.length arguments))

and apply_push (arguments : Ast.expression list) (env : Env.t) :
    Object.t * Env.t =
  if List.length arguments = 2 then
    let arr = List.nth arguments 0 in
    let arr_obj, _ = eval_expression arr env in
    let ele = List.nth arguments 1 in
    let ele, _ = eval_expression ele env in
    match arr_obj with
    | Object.Array arr -> (Object.Array (arr @ [ ele ]), env)
    | _ -> failwith ("Not an array: " ^ Ast.exp_to_string arr)
  else
    failwith
      ("Wrong number of arguments for push: "
      ^ string_of_int (List.length arguments))

and apply_puts (arguments : Ast.expression list) (env : Env.t) :
    Object.t * Env.t =
  let rec r_apply_puts (arguments : Object.t list) (env : Env.t) :
      Object.t * Env.t =
    match arguments with
    | [] -> (null_object, env)
    | h :: t ->
        let () = print_string (Object.inspect h) in
        r_apply_puts t env
  in
  let arguments =
    List.map (function exp -> fst (eval_expression exp env)) arguments
  in
  r_apply_puts arguments env

and eval_index_expression (arr : Ast.expression) (index : Ast.expression)
    (env : Env.t) : Object.t =
  let arr_obj, _ = eval_expression arr env in
  let index_obj, _ = eval_expression index env in
  match (arr_obj, index_obj) with
  | Object.Array arr, Object.Integer index -> (
      match List.nth_opt arr index with
      | Some value -> value
      | None -> null_object)
  | Object.Hash h, Object.Integer index -> (
      match Object.IntMap.find_opt index h.intMap with
      | Some value -> value
      | None -> null_object)
  | Object.Hash h, Object.String index -> (
      match Object.StringMap.find_opt index h.stringMap with
      | Some value -> value
      | None -> null_object)
  | Object.Hash h, Object.Boolean true -> h.trueItem
  | Object.Hash h, Object.Boolean false -> h.falseItem
  | Object.Array _, _ ->
      failwith ("Index is not an integer: " ^ Ast.exp_to_string index)
  | Object.Hash _, _ ->
      failwith ("Index is not supported: " ^ Ast.exp_to_string index)
  | _, _ -> failwith ("Index not supported for: " ^ Ast.exp_to_string arr)

and define_macros (prog : Ast.program) (env : Env.t) : Ast.program * Env.t =
  let rec r_define_macros (stms : Ast.statement list) (env : Env.t) :
      Ast.statement list * Env.t =
    match stms with
    | h :: t -> (
        let stms, env = r_define_macros t env in
        match h with
        | Ast.Let { name; value = Ast.MacroLiteral { parameters; body } } ->
            let name = Ast.exp_to_string name in
            let macro_obj = Object.Macro { parameters; body } in
            let env = Env.set name macro_obj env in
            (stms, env)
        | _ -> (h :: stms, env))
    | [] -> (stms, env)
  in
  let stms, env = r_define_macros prog.statements env in
  (Ast.{ statements = stms }, env)

and expand_macros (prog : Ast.program) (env : Env.t) : Ast.program =
  let modify_func (exp : Ast.expression) : Ast.expression =
    match exp with
    | Ast.Call { fn; arguments } -> (
        let obj, _ = eval_expression fn env in
        match obj with
        | Object.Macro { parameters; body } -> (
            let arguments =
              List.map (function arg -> Object.Quote arg) arguments
            in
            let extended_env = extend_macro_env parameters arguments env in
            let evaluated, _ = eval_statement body extended_env in
            match evaluated with
            | Object.Quote exp -> exp
            | _ -> failwith "We only support returning AST-nodes from macros")
        | _ -> exp)
    | _ -> exp
  in
  Ast.modify_program modify_func prog

and extend_macro_env (parameters : Ast.expression list)
    (arguments : Object.t list) (env : Env.t) =
  let extended_env = Env.new_enclosed_env env in
  let rec r_extend (parameters : Ast.expression list)
      (arguments : Object.t list) (extended_env : Env.t) : Env.t =
    match (parameters, arguments) with
    | [], [] -> extended_env
    | h1 :: t1, h2 :: t2 ->
        let parameter =
          match h1 with
          | Ast.Identifier name -> name
          | _ -> failwith (Ast.exp_to_string h1 ^ " is not Identifier")
        in
        let extended_env = Env.set parameter h2 extended_env in
        r_extend t1 t2 extended_env
    | _, _ ->
        failwith "Number of parameters and number of arguments are not equal"
  in
  r_extend parameters arguments extended_env
