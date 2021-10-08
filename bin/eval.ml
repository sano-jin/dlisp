(* eval.ml *)

open Syntax
open Util

(** Master branch を辿りながら順実行する．
辿ってきた node は sub stream 化して逆順につなぐ．
*)
let rec traverse_main_stream parent_ref this_ref =
  match !this_ref with
  | Sub _ ->
      failwith @@ "substream should not be reached from main stream: "
      ^ string_of_history this_ref
  | Main (id, (addr, value), next_ref_opt) -> (
      prerr_endline @@ "    . traversing main stream: "
      ^ string_of_history this_ref;
      let old_value = !addr in
      addr := value;
      this_ref := Sub (id, (addr, old_value), parent_ref);
      match next_ref_opt with
      | None ->
          prerr_endline "    .";
          ()
      | Some next_ref -> traverse_main_stream this_ref next_ref)

(** 履歴を辿る．
Master branch に辿り着いたら，traverse_main_stream を実行し，
その後に sub stream を順実行しながらこれを main stream 化する．
*)
let rec traverse_history next_ref this_ref =
  match !this_ref with
  | Sub (id, (addr, value), parent_ref) ->
      traverse_history (Some this_ref) parent_ref;
      let old_value = !addr in
      addr := value;
      this_ref := Main (id, (addr, old_value), next_ref)
  | Main (id, (addr, value), old_next_ref_opt) -> (
      match old_next_ref_opt with
      | None -> ()
      | Some old_next_ref ->
          (* TODO: ここの更新操作は本当に必要なのか？ *)
          traverse_main_stream this_ref old_next_ref;
          this_ref := Main (id, (addr, value), next_ref))
(*
   | Main ((addr, value), old_next_ref_opt) -> (
       match old_next_ref_opt with
       | None -> ()
       | Some old_next_ref ->
           traverse_main_stream this_ref old_next_ref;
           this_ref := Main ((addr, value), next_ref))
*)

(** The evaluator *)
let rec eval env = function
  | Atom id ->
      let value =
        match List.assoc_opt id env with
        | Some value -> value
        | None -> failwith @@ "variable " ^ id ^ " not found"
      in
      (match value with
      | DList (_, _, history_ref) as this_dlist ->
          prerr_endline @@ "    >>> " ^ id ^ " = ";
          prerr_endline @@ "    >>>   updating: "
          ^ string_of_history history_ref;

          traverse_history None history_ref;

          prerr_endline @@ "    >>>   updated : "
          ^ string_of_history history_ref;
          prerr_endline @@ "    >>>   ---> " ^ string_of_value this_dlist
      | _ -> ());
      value
  | Number _ as number -> number
  | Bool _ as bool -> bool
  | String _ as string -> string
  | DList (head_ref, _, history_ref) as this_dlist -> (
      (* 履歴を辿って，データを更新 *)
      traverse_history None history_ref;

      (* 扱いやすさのために，差分リストを OCaml のリストに変換する．
         tail_ref の情報は用いずに，[] まで辿っている
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
      | Atom "begin" :: (_ :: _ as args) ->
          List.fold_left (eval env <.. const2) (Atom "void") args
      | [ Atom "print"; arg ] ->
          let value = eval env arg in
          print_endline @@ string_of_value value;
          value
      | [ Atom "let"; binds; body ] ->
          let binding_of bind =
            match extract_dlist bind with
            | [ Atom id; value ] -> (id, eval env value)
            | _ -> failwith @@ "error: invalid binding"
          in
          let env = List.map binding_of (extract_dlist binds) @ env in
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
              @@ "both argments are expected to be lists with an append")
      | Atom op :: _ -> failwith @@ "operation " ^ op ^ " not implemented"
      | _ -> failwith @@ string_of_value this_dlist ^ " is not implemented")
