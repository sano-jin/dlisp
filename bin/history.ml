(** 履歴管理を行うモジュール．
traverse_history のみ外部に公開しておけば良い（差分リストの場合は評価の前にこれを呼ぶ）．
*)

open Syntax

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
      | None -> prerr_endline "    ."
      | Some next_ref -> traverse_main_stream this_ref next_ref)

(** 履歴を辿る．
この関数のみ外部に公開しておけば良い（差分リストの場合は評価の前にこれを呼ぶ）．
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
  | Main (id, (addr, value), old_next_ref_opt) ->
      (match old_next_ref_opt with
      | None -> ()
      | Some old_next_ref -> traverse_main_stream this_ref old_next_ref);
      this_ref := Main (id, (addr, value), next_ref)
