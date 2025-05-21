val eval : Ast.program -> Object.env_t -> Object.t * Object.env_t
val define_macros : Ast.program -> Object.env_t -> Ast.program * Object.env_t
val expand_macros : Ast.program -> Object.env_t -> Ast.program
