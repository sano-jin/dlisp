(* Parser *)
     
%{
  open Syntax
%}

%token <string> ATOM	(* x, y, abc, ... *)
%token <string> STRING	(* 'str', ... *)
%token <int> INT	(* 0, 1, 2, ...  *)

(* operators *)
%token QUOTE		(* '\'' *)
		       
(* Parentheses *)
%token LPAREN		(* '(' *)
%token RPAREN		(* ')' *)

(* reserved names *)
%token TRUE		(* "true"   *)
%token FALSE		(* "false"  *)

(* End of file *)
%token EOF 

%start main
%type <Syntax.value> main

%%

(* Main part must end with EOF (End Of File) *)
main:
  | exp EOF { $1 }
;

(* list *)
list_inner:
  | exp { [$1] }
  | exp list_inner { $1::$2 }
;
	

(* expression *)
exp:
  | ATOM
    { Atom $1 }
    
  | INT
    { Number $1 }
  
  | TRUE
    { Bool true }
    
  | FALSE
    { Bool false }
  
  | STRING
    { String $1 }

  | LPAREN list_inner RPAREN
    { new_dlist $2 }

  | LPAREN RPAREN
    { new_dlist [] }

  | QUOTE exp
    { new_dlist [Atom "quote"; $2] }
;


