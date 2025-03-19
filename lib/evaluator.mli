val eval : Ast.program -> Env.t -> Object.t * Env.t
val define_macros : Ast.program -> Env.t -> Ast.program * Env.t
val expand_macros : Ast.program -> Env.t -> Ast.program
