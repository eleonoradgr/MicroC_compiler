
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | TYPE_INT
  | TYPE_FLOAT
  | TYPE_CHAR
  | TYPE_BOOL
  | TRUE
  | TIMES
  | STRING of (string)
  | SEMICOLON
  | RPAR
  | RETURN
  | RBRACK
  | RBRACE
  | PLUSASS
  | PLUS
  | OR
  | NULL
  | NOT
  | NEQ
  | MULTASS
  | MODASS
  | MOD
  | MINUSASS
  | MINUS
  | LPAR
  | LESS
  | LEQ
  | LBRACK
  | LBRACE
  | INT of (int)
  | INC
  | IF
  | ID of (string)
  | GREATER
  | GEQ
  | FOR
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DO
  | DIVASS
  | DIV
  | DEC
  | COMMA
  | CHAR of (char)
  | ASS
  | AND
  | ADDR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
