(** Parser の top-level *)

open Lexing
open Util

(** parse : string -> stmt *)
let parse_with_error filename str =
  let lexbuf = Lexing.from_string @@ "\n" ^ str ^ "\n" in
  try Parser.main Lexer.token lexbuf with
  | Lexer.SyntaxError msg ->
      prerr_endline @@ msg ^ " in " ^ filename;
      exit (-1)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d in %s\n"
        (pred pos.pos_lnum)
        (pos.pos_cnum - pos.pos_bol + 1)
        filename;
      exit (-1)

let read_and_parse filename = parse_with_error filename @@ read_file filename
