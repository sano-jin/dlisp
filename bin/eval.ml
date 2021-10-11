(** The evaluator *)

open Syntax
open Util
open History

(** The evaluator *)
let rec eval env = function
  | Atom id ->
      let value =
        match List.assoc_opt id !env with
        | Some value -> !value
        | None -> failwith @@ "variable " ^ id ^ " not found"
      in
      (match value with
      | DList (_, _, history_ref) as this_dlist ->
          (* 差分リストだった場合は履歴を更新する *)
          prerr_endline @@ "    >>> " ^ id ^ " = ";
          prerr_endline @@ "    >>>   updating: "
          ^ string_of_history history_ref;

          update history_ref;

          prerr_endline @@ "    >>>   updated : "
          ^ string_of_history history_ref;
          prerr_endline @@ "    >>>   ---> " ^ string_of_value this_dlist
      | _ -> ());
      value
  | (Number _ | Bool _ | String _ | Closure _) as value -> value
  | DList (head_ref, _, history_ref) as this_dlist -> (
      (* 履歴を辿って，データを更新 *)
      update history_ref;

      (* 扱いやすさのために，差分リストを OCaml のリストに変換する．
         tail_ref の情報は用いずに，Nil まで辿っている
      *)
      let list = list_of_node_ref head_ref in
      let eval_binop_num f unit args =
        let arg_vals = List.map (extract_number <. eval env) args in
        List.fold_left f unit arg_vals
      in
      match list with
      | [] -> this_dlist
      | Atom "+" :: args -> Number (eval_binop_num ( + ) 0 args)
      | Atom "*" :: args -> Number (eval_binop_num ( * ) 1 args)
      | [ Atom "quote"; value ] -> value
      | [ Atom "if"; cond; exp1; exp2 ] -> (
          match eval env cond with
          | Bool bool -> if bool then eval env exp1 else eval env exp2
          | value -> failwith @@ "expected boolean: " ^ string_of_value value)
      | [ Atom "car"; value ] -> (
          match eval env value with
          | DList (head_ref, _, history_ref) -> (
              update history_ref;
              match !head_ref with
              | Nil -> failwith @@ "cannot car with nil"
              | Cons (value, _) -> eval env value)
          | value -> failwith @@ "cannot car with " ^ string_of_value value)
      | [ Atom "null?"; value ] -> (
          match eval env value with
          | DList (head_ref, _, history_ref) -> (
              update history_ref;
              match !head_ref with Nil -> Bool true | Cons _ -> Bool false)
          | _ -> Bool false)
      | [ Atom "cdr"; value ] -> (
          match eval env value with
          | DList (head_ref, tail_ref, history_ref) -> (
              update history_ref;
              match !head_ref with
              | Nil -> failwith @@ "cannot car with nil"
              | Cons (_, next_ref) -> DList (next_ref, tail_ref, history_ref))
          | value -> failwith @@ "cannot cdr with " ^ string_of_value value)
      | [ Atom "cons"; head; tail ] -> (
          match (eval env head, eval env tail) with
          | head, DList (head_ref, tail_ref, history_ref) ->
              update history_ref;
              DList (ref (Cons (head, head_ref)), tail_ref, history_ref)
          | _, tail -> failwith @@ "cannot cons with " ^ string_of_value tail)
      | [ Atom "lambda"; DList (arg_head_ref, _, arg_history_ref); body ] ->
          update arg_history_ref;
          let args = list_of_node_ref arg_head_ref in
          let arg_strs = List.map extract_variable args in
          Closure (arg_strs, body, env)
      | Atom "begin" :: (_ :: _ as args) ->
          List.fold_left (eval env <.. const2) (Atom "void") args
      | [ Atom "print"; arg ] ->
          let value = eval env arg in
          print_endline @@ string_of_value value;
          value
      | [ Atom "let"; binds; body ] ->
          let binding_of bind =
            match extract_dlist bind with
            | [ Atom id; value ] -> (id, ref (eval env value))
            | _ -> failwith @@ "error: invalid binding"
          in
          let env = ref (List.map binding_of (extract_dlist binds) @ !env) in
          eval env body
      | [ Atom "letrec"; binds; body ] ->
          let binding_of bind =
            match extract_dlist bind with
            | [ Atom id; value ] -> (id, value)
            | _ -> failwith @@ "invalid binding: " ^ string_of_value bind
          in
          let bindings = List.map binding_of @@ extract_dlist binds in
          let eval_binding (var, exp) =
            env := (var, ref (eval env exp)) :: !env
          in
          List.iter eval_binding bindings;
          eval env body
      | [ Atom "++"; list1; list2 ] -> (
          match (eval env list1, eval env list2) with
          | ( DList (head_ref1, tail_ref1, history_ref1),
              DList (head_ref2, tail_ref2, _) ) -> (
              match !history_ref1 with
              | Main (id, operation, None) ->
                  let this_history_ref =
                    ref @@ Main (unique (), (tail_ref1, !tail_ref1), None)
                  in
                  history_ref1 := Main (id, operation, Some this_history_ref);
                  tail_ref1 := !head_ref2;
                  DList (head_ref1, tail_ref2, this_history_ref)
              | _ ->
                  failwith
                  @@ "error: expected to be the latest version of the list")
          | _ ->
              failwith
              @@ "both argments are expected to be lists with an append op")
      | f :: args -> (
          match eval env f with
          | Closure (vars, body, new_env) ->
              let arg_values = List.map (ref <. eval env) args in
              let new_env = ref (List.combine vars arg_values @ !new_env) in
              eval new_env body
          | f -> failwith @@ string_of_value f ^ " is expected to be a function"
          ))
