(* eval.ml *)

open Syntax
open Util

(** Master branch を辿りながら順実行する．
辿ってきた node は sub stream 化して逆順につなぐ．
*)
let rec traverse_main_stream parent_ref this_ref =
  match !this_ref with
  | Sub ((addr, value), _) ->
      failwith @@ "substream should not be reached from main stream: Sub ("
      ^ string_of_node !addr ^ " := " ^ string_of_node value ^ ")"
  | Main ((addr, value), next_ref_opt) -> (
      (*
      prerr_endline @@ "traversing main stream: Main (" ^ string_of_node !addr
      ^ " := " ^ string_of_node value ^ ")";
  *)
      let old_value = !addr in
      addr := value;
      this_ref := Sub ((addr, old_value), parent_ref);
      match next_ref_opt with
      | None -> ()
      | Some next_ref -> traverse_main_stream this_ref next_ref)

(** 履歴を辿る．
Master branch に辿り着いたら，traverse_main_stream を実行し，
その後に sub stream を順実行しながらこれを main stream 化する．
*)
let rec traverse_history next_ref this_ref =
  match !this_ref with
  | Sub ((addr, value), parent_ref) ->
      (*
      prerr_endline @@ "traversing: Sub (" ^ string_of_node !addr ^ " := "
      ^ string_of_node value ^ ")";
  *)
      traverse_history (Some this_ref) parent_ref;
      let old_value = !addr in
      addr := value;
      this_ref := Main ((addr, old_value), next_ref)
  | Main ((addr, value), next_ref_opt) -> (
      (*
      prerr_endline @@ "traversing: Main (" ^ string_of_node !addr ^ " := "
      ^ string_of_node value ^ ")";
  *)
      match next_ref_opt with
      | None -> ()
      | Some next_ref ->
          traverse_main_stream this_ref next_ref;
          this_ref := Main ((addr, value), None))

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
          prerr_endline @@ ">>> updating: " ^ string_of_history history_ref;

          traverse_history None history_ref;

          prerr_endline @@ ">>> updated: "
          ^ string_of_history history_ref
          ^ " this = " ^ string_of_value this_dlist
      | _ -> ());
      value
  | Number _ as number -> number
  | Bool _ as bool -> bool
  | String _ as string -> string
  | DList (head_ref, _, history_ref) as this_dlist -> (
      prerr_endline @@ ">>> updating: " ^ string_of_history history_ref;

      (* 履歴を辿って，データを更新 *)
      traverse_history None history_ref;

      prerr_endline @@ ">>> updated: "
      ^ string_of_history history_ref
      ^ " this = " ^ string_of_value this_dlist;

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
          (*
                prerr_endline @@ string_of_value this_dlist;
  *)
          match (eval env list1, eval env list2) with
          | ( DList (head_ref1, tail_ref1, history_ref1),
              DList (head_ref2, tail_ref2, _) ) -> (
              (*
              prerr_endline @@ ">>> " ^ string_of_history history_ref1;
  *)
              match !history_ref with
              | Main (operation, None) ->
                  let this_history_ref =
                    ref @@ Main ((tail_ref1, !tail_ref1), None)
                  in
                  history_ref1 := Main (operation, Some this_history_ref);
                  tail_ref1 := !head_ref2;

                  prerr_endline @@ "... " ^ string_of_history this_history_ref;
                  prerr_endline @@ "... " ^ string_of_history history_ref1;

                  DList (head_ref1, tail_ref2, this_history_ref)
              | _ ->
                  failwith
                  @@ "error: expected to be the latest version of the list")
          | _ ->
              failwith
              @@ "both argments are expected to be lists with an append")
      | Atom op :: _ -> failwith @@ "operation " ^ op ^ " not implemented"
      | _ -> failwith @@ string_of_value this_dlist ^ " is not implemented")

(*
          (
      (* 自分が読むための更新を行い，巻き戻しのための関数を返す *)
      let rewinder =
        match uf_find_opt my_id with
        | None -> id (* not sharing *)
        | Some owner_id ->
            if owner_id = my_id then id (* Do nothing, if I am the owner *)
            else (
              (* 更新を行い，オーナーを自分に設定，後でこれらを戻すための関数を返す *)
              (* オーナーを自分に設定 *)
              uf_update_owner owner_id my_id;
              (* 自分が更新する前の tail のデータを保存しておく *)
              let tail = !tail_ref in
              (* 自分のデータで更新する *)
              tail_ref := redir;
              (* 巻き戻しのための関数．引数をそのまま返す *)
              fun value ->
                (* オーナーを元のものに戻す *)
                uf_update_owner owner_id my_id;
                (* 自分が更新する前の tail のデータに戻す *)
                tail_ref := tail;
                value)
      in
      match !head_ref with
      | Nil -> Nil
      | Cons (value_ref, node_ref) ->
          (match !value with Atom "+" -> list_of_node_refs) node_ref
          * hge higeh hige)


*)
