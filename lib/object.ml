module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

type t =
  | Integer of int
  | Boolean of bool
  | String of string
  | Array of t list
  | Hash of {
      stringMap : t StringMap.t;
      intMap : t IntMap.t;
      trueItem : t;
      falseItem : t;
      keys : t list;
      values : t list;
    }
  | ReturnValue of t
  | Function of { parameters : Ast.expression list; body : Ast.statement }
  | Builtin of builtin_function
  | Null

and builtin_function = Len | First | Last | Rest | Push | Puts

let sub_if_longer_than (str : string) (length : int) : string =
  if String.length str >= length then
    String.sub str 0 (String.length str - length)
  else str

let rec inspect (obj : t) =
  match obj with
  | Integer value -> string_of_int value ^ "\n"
  | Boolean value -> string_of_bool value ^ "\n"
  | String value -> value ^ "\n"
  | Array objs ->
      let objs =
        List.fold_left
          (fun acc x ->
            let x = sub_if_longer_than (inspect x) 1 in
            acc ^ x ^ ", ")
          "" objs
      in
      let objs = sub_if_longer_than objs 2 in
      "[" ^ objs ^ "]" ^ "\n"
  | Hash { keys; values; _ } ->
      let rec r_hash_string (keys : t list) (values : t list) (acc : string) :
          string =
        match (keys, values) with
        | [], [] -> acc
        | h1 :: [], h2 :: [] ->
            let key = sub_if_longer_than (inspect h1) 1 in
            let value = sub_if_longer_than (inspect h2) 1 in
            acc ^ key ^ " : " ^ value
        | h1 :: t1, h2 :: t2 ->
            let key = sub_if_longer_than (inspect h1) 1 in
            let value = sub_if_longer_than (inspect h2) 1 in
            r_hash_string t1 t2 (acc ^ key ^ " : " ^ value ^ ", ")
        | _, _ -> failwith "hash to string error"
      in
      "{ " ^ r_hash_string keys values "" ^ " }\n"
  | ReturnValue obje -> inspect obje
  | Function { parameters; body } ->
      let params =
        List.fold_left
          (fun acc x -> acc ^ Ast.exp_to_string x ^ ", ")
          "" parameters
      in
      let params = sub_if_longer_than params 2 in
      "fn(" ^ params ^ ")" ^ Ast.stm_to_string body ^ "\n"
  | Builtin _ -> "Builtin function\n"
  | Null -> "null\n"

and get_builtin (name : string) : t option =
  match name with
  | "len" -> Some (Builtin Len)
  | "first" -> Some (Builtin First)
  | "last" -> Some (Builtin Last)
  | "rest" -> Some (Builtin Rest)
  | "push" -> Some (Builtin Push)
  | "puts" -> Some (Builtin Puts)
  | _ -> None
