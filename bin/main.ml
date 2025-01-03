let () = print_string ">> " in
let input = read_line () in
(*let input = "let x = 5;" in*)
let lex = Lexer.init input in
(*let () = print_endline (Lexer.show lex) in*)
let par = Parser.init lex in
print_string (Parser.show par)
