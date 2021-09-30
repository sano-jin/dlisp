(* Parser *)
     
%{
  open Syntax
  open Util
%}

%token <string> ATOM	(* x, y, abc, ... *)
%token <string> STRING	(* 'str', ... *)
%token <int> INT	(* 0, 1, 2, ...  *)

(* operators *)
%token DOT		(* '.' *)
%token QUOTE		(* '\'' *)
		       
(* Parentheses *)
%token LPAREN		(* '(' *)
%token RPAREN		(* ')' *)

(* reserved names *)
%token TRUE		(* "true"   *)
%token FALSE		(* "false"  *)

(* End of file *)
%token EOF 

(*
(* Operator associativity *)
%nonassoc DOT
%nonassoc LPAREN
*)

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
    { let nil_ref = ref Nil in
      DList (unique (), dlist_of_list $2 nil_ref, nil_ref, [])
    }

  | LPAREN list_inner DOT exp RPAREN
    { let tail_ref = ref (Dot (ref $4)) in
      DList (unique (), dlist_of_list $2 tail_ref, tail_ref, [])
    }

  | QUOTE exp
    { let nil_ref = ref Nil in
      DList (unique (), dlist_of_list [Atom "quote"; $2] nil_ref, nil_ref, [])
    }
;


