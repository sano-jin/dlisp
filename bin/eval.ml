(* eval.ml *)

open Syntax
open Util

type uftree = (int * int) list ref
(** A very simple (and not efficient) union find tree.
    [(dlist_id, union_find_id) list].
    the [union_find_id] is [dlist_id] if it is the owner.
*)

let uftree = ref []

(** find the corresponding union_find_id *)
let uf_find_opt dlist_id = List.assoc_opt dlist_id !uftree

(** unify the union_find_id x with y *)
let uf_unify x y =
  update_ref (List.map @@ second @@ fun z -> if z = x then y else z) uftree

(** owner を update する．
    uf_unify と同じ関数だが，あと後で効率よく実装する際の混乱を避けるため，分けておく
*)
let uf_update_owner = uf_unify

(** The evaluator *)
let rec eval env uftree =
  let list_of_node_refs node_ref = node_ref in

  function
  | Atom _ as atom -> const atom eval
  | Number _ as number -> number
  | Bool _ as bool -> bool
  | String _ as string -> string
  | DList (my_id, head_ref, tail_ref, redir) -> (
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
