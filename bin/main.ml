(*let input = "let a = 5;a*5" in*)
(*let lex = Lexer.init input in*)
(*let par = Parser.init lex in*)
(*let _, prog = Parser.parse_program par in*)
(*let env = Env.new_env in*)
(*let obj, _ = Evaluator.eval prog env in*)
(*print_endline (Object.inspect obj)*)
let rec repl (env : Env.t) : Env.t =
  let () = print_string ">> " in
  let input = read_line () in
  let lex = Lexer.init input in
  let par = Parser.init lex in
  let _, prog = Parser.parse_program par in
  let obj, env = Evaluator.eval prog env in
  let () = print_string (Object.inspect obj) in
  repl env
in
let env = Env.new_env in
repl env
