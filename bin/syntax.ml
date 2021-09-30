(** syntax.ml *)

open Util.ListExtra

(** coherence *)
type coherence = M | E | S | I

(** value *)
type value =
  | Atom of string  (** variable e.g. x *)
  | DList of int * node ref * node ref * node
  | Number of int  (** integer value e.g. 17 *)
  | Bool of bool  (** boolean value e.g. true *)
  | String of string  (** string value e.g. "hellow world!" *)

and node = Cons of value * node ref | Nil

type env = (string * value) list
(** environment e.g. [("x", 1); ("y", 2)]*)

let rec string_of_value = function
  | Atom atom -> atom
  | DList (_, node_ref, _, _) ->
      "(" ^ String.concat " " (strings_of_node !node_ref) ^ ")"
  | Number number -> string_of_int number
  | Bool bool -> string_of_bool bool
  | String string -> string

and strings_of_node = function
  | Cons (value, node_ref) -> string_of_value value :: strings_of_node !node_ref
  | Nil -> []

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
      ^ " is valueected to be a number"

let cons_of value tail = ref (Cons (value, tail))

let dlist_of_list list tail = List.fold_right cons_of list tail

let rec list_of_node_ref node_ref =
  match !node_ref with
  | Cons (value_ref, node_ref) -> value_ref :: list_of_node_ref node_ref
  | Nil -> []
