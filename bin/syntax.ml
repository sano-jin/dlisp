(** syntax.ml *)

open Util
open Util.ListExtra

(** value *)
type value =
  | Atom of string  (** variable e.g. x *)
  | DList of node ref * node ref * history_node ref * int UnionFind.elem
      (** DList (head_ref, tail_ref, history_ref, union_find) *)
  | Number of int  (** integer value e.g. 17 *)
  | Bool of bool  (** boolean value e.g. true *)
  | Closure of string list * value * env  (** closure *)
  | String of string  (** string value e.g. "hellow world!" *)

(** node of difference list *)
and node = Cons of value * node ref | Nil

(** node for the history management *)
and history_node =
  | Main of int * (node ref * node) * history_node ref option
  | Sub of int * (node ref * node) * history_node ref

and env = (string * value ref) list ref
(** environment for the evaluator. e.g. [("x", 1); ("y", 2)] *)

(** DList から OCaml のリストへの変換 *)
let rec list_of_node_ref node_ref =
  match !node_ref with
  | Cons (value_ref, node_ref) -> value_ref :: list_of_node_ref node_ref
  | Nil -> []

(** pretty printer of the values *)
let rec string_of_value = function
  | Atom atom -> atom
  | DList (node_ref, _, _, _) ->
      "("
      ^ String.concat " " (List.map string_of_value @@ list_of_node_ref node_ref)
      ^ ")"
  | Number number -> string_of_int number
  | Bool bool -> string_of_bool bool
  | String string -> string
  | Closure (args, _, _) -> "(lambda (" ^ String.concat " " args ^ ") ...)"

(** pretty printer of the history management tree *)
let rec string_of_history history_ref =
  let string_of_node = function
    | Nil -> "[]"
    | Cons (value, _) -> "[" ^ string_of_value value ^ "|-]"
  in
  match !history_ref with
  | Main (id, (addr, value), None) ->
      string_of_int id ^ ":Main (" ^ string_of_node !addr ^ " <- "
      ^ string_of_node value ^ ")."
  | Main (id, (addr, value), Some next_ref) ->
      string_of_int id ^ ":Main (" ^ string_of_node !addr ^ " <- "
      ^ string_of_node value ^ ")" ^ " -> " ^ string_of_history next_ref
  | Sub (id, (addr, value), prev_ref) ->
      string_of_int id ^ ":Sub (" ^ string_of_node !addr ^ " <- "
      ^ string_of_node value ^ ")" ^ " -> " ^ string_of_history prev_ref

let string_of_env env =
  let string_of_binding (var, value_ref) =
    Printf.sprintf "%s -> %s" var (string_of_value !value_ref)
  in
  string_of_list string_of_binding !env

let type_error value expected =
  failwith @@ "TypeError: " ^ string_of_value value ^ " is expected to be a "
  ^ expected

(** some helper functions *)
let extract_number = function
  | Number number -> number
  | value -> type_error value "number"

let extract_variable = function
  | Atom id -> id
  | value -> type_error value "atom"

(** 値が DList だった場合に，OCaml リストに変換して返す．  *)
let extract_dlist = function
  | DList (head_ref, _, _, _) -> list_of_node_ref head_ref
  | value -> type_error value "dlist"

(** OCaml のリストから新しい差分リストを生成して返す *)
let new_dlist list =
  (* OCaml のリストから Cons への変換 *)
  let dlist_of_list =
    List.fold_right (fun value tail -> ref (Cons (value, tail)))
  in
  let nil_ref = ref Nil in
  let init_nil = Cons (Atom "root", nil_ref) in
  let id = unique () in
  let union_find = UnionFind.make id in
  DList
    ( dlist_of_list list nil_ref,
      nil_ref,
      ref @@ Main (id, (nil_ref, init_nil), None),
      union_find )
