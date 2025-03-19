module StringMap = Map.Make (String)

type t = { store : Object.t StringMap.t; outer : t option }

let (new_env : t) = { store = StringMap.empty; outer = None }

let new_enclosed_env (outer : t) : t =
  { store = StringMap.empty; outer = Some outer }

let rec get (key : string) (env : t) : Object.t option =
  match (StringMap.find_opt key env.store, env.outer) with
  | Some value, _ -> Some value
  | None, Some out -> get key out
  | None, None -> None

let set (key : string) (value : Object.t) (env : t) : t =
  { env with store = StringMap.add key value env.store }

let env_to_string (env : t) : string =
  StringMap.fold
    (fun key value acc -> acc ^ key ^ " : " ^ Object.inspect value)
    env.store ""
