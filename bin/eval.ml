(* eval.ml *)

open Syntax
open Util

(** The evaluator *)
let rec eval _ = function
  | Atom _ as atom -> const atom eval
  | _ as other -> other
