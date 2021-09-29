(** syntax.ml *)

open Util
open Util.ListExtra

(** expression *)
type exp =
  | Var of string  (** variable e.g. x *)
  | IntLit of int  (** integer literal e.g. 17 *)
  | Plus of exp * exp  (** e + e *)
  | Times of exp * exp  (** e * e *)
  | Lt of exp * exp  (** e < e *)
  | And of exp * exp  (** e != e *)
  | Not of exp  (** not e *)
  | Lambda of string list * stmt  (** lambda x, y : {return x + y} *)
  | App of exp * exp list  (** f (x1, ..., xn) *)
  | Let of string * exp * exp  (** bind e.g. x := 1 + 2 * y *)
  | If of exp * stmt * stmt  (** branch e.g. if (1 < x) { x := x + 1 } *)

(** value *)
type value =
  | IntVal of int
  | LambdaVal of string list * stmt * env  (** closure *)

and env = (string * value) list
(** environment e.g. [("x", 1); ("y", 2)]*)

let rec string_of_value = function
  | IntVal i -> string_of_int i
  | LambdaVal (vars, _, _) -> "\\" ^ String.concat " " vars ^ " => ..."

let string_of_env env =
  let string_of_binding (var, value) =
    Printf.sprintf "(%s, %s)" var @@ string_of_value value
  in
  string_of_list string_of_binding env

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be int"
