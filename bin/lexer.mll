(* Lexer *)

{
  open Parser
  exception SyntaxError of string
}

let space = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha
let newline = '\r' | '\n' | "\r\n"
let symbol = ['!' '#' '$' '%' '&' '|' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~' '"']
let alpha_symbol = alpha | symbol
let alnum_symbol = alnum | symbol


(* 改行後のスペースを indent で読んだ後に呼ばれる Lexer *)
rule token = parse
  (* Operators *)
  | '.'               { DOT }

  (* Parentheses *)
  | '('               { LPAREN }
  | ')'               { RPAREN }
  
  (* reserved names *)
  | "#t"            { TRUE }
  | "#f"            { FALSE }

  (* Number *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }
  
  (* Atom *)
  | alpha_symbol alnum_symbol*
    { ATOM (Lexing.lexeme lexbuf) }
    
  (* string *)
  | '"' [^ '\'']* '"'
    { let str = Lexing.lexeme lexbuf in
      STRING (String.sub str 1 @@ String.length str - 2)
    }

  (* end of file *)
  | eof       { EOF }

  (* spaces *)
  | space+    { token lexbuf }

  (* new line *)
  | newline  { Lexing.new_line lexbuf; token lexbuf }

  (* comment *)
  | ";;" [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "Unknown token '%s' near line %d (near characters %d-%d)"
        (Lexing.lexeme lexbuf)
        (pred lexbuf.lex_curr_p.pos_lnum)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      raise @@ SyntaxError message
    }



 
