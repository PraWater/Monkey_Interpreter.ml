let () = print_string ">> " in
let input = read_line () in
let lex = Lexer.init input in
print_endline (Lexer.show lex)
