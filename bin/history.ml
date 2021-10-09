(** 履歴管理を行うモジュール．
update のみ外部に公開しておけば良い（差分リストの場合は評価の前にこれを呼ぶ）．
*)

open Syntax

(** Main stream を辿りながら帰りがけに逆実行する．
辿ってきた node は sub stream 化して逆順につなぐ．
*)
let rec traverse_main_stream parent_ref this_ref =
  match !this_ref with
  | Sub _ ->
      failwith @@ "substream should not be reached from main stream: "
      ^ string_of_history parent_ref
  | Main (id, (addr, value), next_ref_opt) ->
      (match next_ref_opt with
      | None -> ()
      | Some next_ref -> traverse_main_stream this_ref next_ref);
      let old_value = !addr in
      addr := value;
      this_ref := Sub (id, (addr, old_value), parent_ref)

(** 履歴を辿る．
+ Sub stream を上へ辿って行き，
+ Main stream (LCA) に辿り着いたら（ただし，LCA の操作は実行しない），[traverse_main_stream] を実行し，
+ その後帰りがけ順に sub stream を順実行しながらこれを main stream 化する．
*)
let rec traverse_history next_ref_opt this_ref =
  match !this_ref with
  | Sub (id, (addr, value), parent_ref) ->
      traverse_history (Some this_ref) parent_ref;
      let old_value = !addr in
      addr := value;
      this_ref := Main (id, (addr, old_value), next_ref_opt)
  | Main (id, addr_value, old_next_ref_opt) ->
      (match old_next_ref_opt with
      | None -> ()
      | Some old_next_ref -> traverse_main_stream this_ref old_next_ref);
      this_ref := Main (id, addr_value, next_ref_opt)

(** 差分リストを評価する前にはこの関数を実行して，
履歴を辿って差分リストを最新の状態にし，履歴を更新する必要がある．
この関数のみ外部に公開しておけば良い．
*)
let update = traverse_history None
