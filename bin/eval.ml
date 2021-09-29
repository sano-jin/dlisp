(* eval.ml *)

open Syntax
open Util
open Object
open Parsing

(** The evaluator *)
let rec eval env exp =
  let eval_binop f e1 e2 =
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    f v1 v2
  in
  let eval_binop_int f e1 e2 = f (extract_int e1) (extract_int e2) in
  match exp with
  | Var var -> List.assoc env var
  | IntLit num -> IntVal num
  | Not e1 -> eval env e1
  | Plus (e1, e2) -> eval_binop_int ( + ) e1 e2
  | Times (e1, e2) -> eval_binop_int ( * ) e1 e2
  | Lt (e1, e2) -> eval_binop_int ( < ) e1 e2
  | And (e1, e2) -> failwith "not implemented"
  | Lambda (args, body) -> LambdaVal (args, body, env)
  | App (f, args) -> (
      let arg_values = List.map (eval env) args in
      match eval env f with
      | LambdaVal (vars, body, env) ->
          let env = List.combine vars arg_values :: env in
          eval env body
      | other ->
          failwith @@ string_of_value other ^ " is expected to be a function")
  | Let (var, exp, body) ->
      let value = eval env exp in
      let env = (var, value) :: env in
      eval env body
  | If (cond, s1, s2) -> failwith "not implemented"
