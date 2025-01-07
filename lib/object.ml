type t =
  | Integer of int
  | Boolean of bool
  | ReturnValue of t
  | Function of { parameters : Ast.expression list; body : Ast.statement }
  | Null

let rec inspect (obj : t) =
  match obj with
  | Integer value -> string_of_int value ^ "\n"
  | Boolean value -> string_of_bool value ^ "\n"
  | ReturnValue obje -> inspect obje
  | Function { parameters; body } ->
      let params =
        List.fold_left
          (fun acc x -> acc ^ Ast.exp_to_string x ^ ", ")
          "" parameters
      in
      let params = String.sub params 0 (String.length params - 2) in
      "fn(" ^ params ^ ")" ^ Ast.stm_to_string body ^ "\n"
  | Null -> ""
