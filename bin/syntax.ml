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

and node = Cons of value * node ref | Nil

and history_node =
  | Main of int * (node ref * node) * history_node ref option
  | Sub of int * (node ref * node) * history_node ref

and env = (string * value ref) list ref
(** environment e.g. [("x", 1); ("y", 2)]*)

let rec string_of_value = function
  | Atom atom -> atom
  | DList (node_ref, _, _, _) ->
      "(" ^ String.concat " " (strings_of_node !node_ref) ^ ")"
  | Number number -> string_of_int number
  | Bool bool -> string_of_bool bool
  | String string -> string
  | Closure (args, _, _) -> "(lambda (" ^ String.concat " " args ^ ") ...)"

and strings_of_node = function
  | Cons (value, node_ref) -> string_of_value value :: strings_of_node !node_ref
  | Nil -> []

let string_of_node = function
  | Nil -> "[]"
  | Cons (value, _) -> "[" ^ string_of_value value ^ "|-]"

let rec string_of_history history_ref =
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
  let string_of_binding (var, value) =
    Printf.sprintf "(%s, %s)" var @@ string_of_value value
  in
  string_of_list string_of_binding env

(** some helper functions *)
let extract_number = function
  | Number number -> number
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be a number"

let extract_variable = function
  | Atom id -> id
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be a variable"

let cons_of value tail = ref (Cons (value, tail))

let dlist_of_list list tail = List.fold_right cons_of list tail

let rec list_of_node_ref node_ref =
  match !node_ref with
  | Cons (value_ref, node_ref) -> value_ref :: list_of_node_ref node_ref
  | Nil -> []

(** 値が DList だった場合に，OCaml リストに変換して返す．  *)
let extract_dlist = function
  | DList (head_ref, _, _, _) -> list_of_node_ref head_ref
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be a dlist"

let new_empty_dlist () =
  let nil_ref = ref Nil in
  let init_nil = Cons (Atom "root", nil_ref) in
  let id = unique () in
  let union_find = UnionFind.make id in
  DList
    (nil_ref, nil_ref, ref @@ Main (id, (nil_ref, init_nil), None), union_find)
