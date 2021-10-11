(* main.ml *)

open Eval
open Syntax
open Parsing

let () =
  let value = eval (ref []) @@ read_and_parse Sys.argv.(1) in
  print_endline @@ string_of_value value
