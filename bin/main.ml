(* main.ml *)

open Eval
open Syntax

let () =
  let value = eval [] @@ read_and_parse Sys.argv.(1) "__main__" in
  print_endline @@ string_of_value value
