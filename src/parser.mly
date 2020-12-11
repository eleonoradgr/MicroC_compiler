/*
* MicroC Parser specification
*/

%{
    open Ast
    open Util

    (* Define here your utility functions *)

    let rec findtype t lenx dim=
      match lenx with
      | 0 ->( if dim == -1 then
                t  
              else
                match dim with
                | 0 -> TypA( t , None )
                | n when n > 0 -> TypA( t , Some(n))
                | _ -> raise( Syntax_error "negative array")
            )
      | l -> TypP( findtype t (l-1) dim )

%}

/* Tokens declarations */

%token EOF
%token <int> INT 
%token <char> CHAR
%token TRUE FALSE
%token NULL VOID
%token TYPE_INT
%token TYPE_CHAR
%token TYPE_BOOL
%token <string> ID
%token RETURN WHILE FOR IF ELSE 
%token PLUS MINUS DIV MOD
%token TIMES "*"
%token EQ NE LESS GREATER LEQ GEQ
%token OR AND NOT
%token ASS 
%token ADDR 
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAR
%token RPAR
%token COLON
%token SEMICOLON ";"
%token COMMA

/* Precedence and associativity specification */

%left OR
%left AND
%left EQ NE
%nonassoc  LESS GREATER LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT
%nonassoc LPAR
%nonassoc UMINUS
%nonassoc POINTER
%nonassoc THEN
%nonassoc ELSE
%nonassoc ACC
%nonassoc ASS

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  |  f=list(topdec) EOF       {Prog(f)}
  | error
    { raise (Syntax_error "error in program") }
;

topdec:
  | v=var SEMICOLON               
    { let first xy = match xy with (x, y) -> x in
      let second xy = match xy with (x, y) -> y in
      {loc= $loc ; node=Vardec(first v, second v); id=0} }
  | f=fund
    { {loc= $loc ; node=f; id=0} }
;

fund:
/*| t=types_fun  id=ID LPAR par=separated_list(COMMA , var) RPAR b= block
      { Fundecl({typ = t; fname = id ; formals = par; body = b}) }*/
  | VOID id=ID LPAR par=separated_list(COMMA, var) RPAR b=block
      { Fundecl({typ = TypV; fname = id ; formals = par; body = b}) }
  | v=var LPAR par=separated_list(COMMA, var) RPAR b=block
      { let first xy = match xy with (x, y) -> x in
        let second xy = match xy with (x, y) -> y in
        Fundecl({typ = first v; fname = second v; formals = par; body = b}) }
; 

var:
  | t=types_dec x=list("*") id=ID
      {let f = (findtype t (List.length x) (-1)) in (f,id)}
  | t=types_dec x=list("*") id=ID LBRACK RBRACK                     
      {let f = (findtype t (List.length x) 0)  in (f,id)}
  | t=types_dec x=list("*") id=ID LBRACK i=INT RBRACK               
      {let f = (findtype t (List.length x) i)  in (f,id)}
  | LPAR v=var RPAR                                           
      {v}
;


types_fun:
  | t=types_dec
    { t } 
  | VOID
    { TypV }
;

types_dec:
  | TYPE_INT
    { TypI }
  | TYPE_BOOL
    { TypB }
  | TYPE_CHAR
    { TypC }
;

block:
  | LBRACE b=list(inblock) RBRACE
    { {loc = $loc; node = Block(b); id = 0 }}
;

inblock:
  | v=var SEMICOLON
    { let first xy = match xy with (x, y) -> x in
      let second xy = match xy with (x, y) -> y in
      {loc = $loc; node = Dec(first v, second v); id = 0}}
  | s=stm
    {{loc = $loc; node = Stmt(s); id = 0}}
;

stm:
  | IF LPAR e=ex RPAR b=stm  %prec THEN
    {let emptyb = {loc = $loc; node = Block([]); id = 0 } in
    {loc = $loc; node = If(e,b,emptyb); id = 0 }}
  | IF LPAR e=ex RPAR b1=stm ELSE b2=stm 
    {{loc = $loc; node = If(e,b1,b2); id = 0 }}
  /*| FOR LPAR a1=acc ASS e1=ex ";" e=ex ";" a2=acc ASS e2=ex RPAR b=block /*mettere a2=e2 nel blocco*/
  /*  { let new_blocK = match b.node with
    |Block(x)   ->    {loc=b.loc; 
                      node= Block(x @ [{loc = $loc; node = Stmt({loc= b.loc; node=Expr({loc=$loc; node=Assign(a2,e2); id =0}); id=0}); id=0}]);
                      id = 0} 
    |_          ->    raise (Syntax_error "errore in exp")  in
      {loc = $loc;
      node = Block([{loc=$loc;
                    node=Stmt({loc = $loc; node = Expr({loc=$loc; node=Assign(a1,e1); id =0}); id = 0});
                    id = 0};
                    {loc=$loc;
                    node=Stmt({loc = $loc; node = While(e,new_blocK); id = 0 });
                    id = 0}]);
      id = 0}} */
  | FOR LPAR a1=acc ASS e1=ex ";" e=ex ";" a2=acc ASS e2=ex RPAR s=stm
    {
     let new_block = { loc=$loc;
                       node= Block([{loc = $loc; node=Stmt(s); id=0};{loc = $loc; node = Stmt({loc= $loc; node=Expr({loc=$loc; node=Assign(a2,e2); id =0}); id=0}); id=0}]); 
                       id=0;
                      } in
      {loc = $loc;
      node = Block([{loc=$loc;
                    node=Stmt({loc = $loc; node = Expr({loc=$loc; node=Assign(a1,e1); id =0}); id = 0});
                    id = 0};
                    {loc=$loc;
                    node=Stmt({loc = $loc; node = While(e,new_block); id = 0 });
                    id = 0}]);
      id = 0}
    }
  | FOR LPAR ";" e=ex ";" RPAR s=stm
   {{loc = $loc; node = While(e,s); id = 0 }}
  | WHILE LPAR e=ex RPAR s=stm
    {{loc = $loc; node = While(e,s); id = 0 }}
  | id=ID LPAR l=separated_list(COMMA ,ex ) RPAR SEMICOLON
    {{loc = $loc; node = Expr({loc=$loc; node=Call(id,l); id =0}); id = 0}}
  | a=acc ASS e=ex SEMICOLON
    {{loc = $loc; node = Expr({loc=$loc; node=Assign(a,e); id =0}); id = 0}} 
  | RETURN e = option( ex ) SEMICOLON
    {{loc = $loc; node = Return(e); id = 0 }}
  | b=block
    {b}
;

elsestm:
  | ELSE s=stm
    {s}

acc:
  | x=ID
    {{loc = $loc; node = AccVar(x); id = 0}}
  | "*" a=acc %prec POINTER
    {{loc = $loc; node = AccDeref({loc = $loc; node = Access(a); id = 0}); id = 0}}
  | x=ID LBRACK ex2=ex RBRACK
    {{loc = $loc; node = AccIndex({loc = $loc; node = AccVar(x); id = 0},ex2); id = 0}}

ex:
  | a=acc %prec ACC
    {{loc=$loc; node=Access(a); id=0}}
  | a=acc ASS e=ex 
    {{loc=$loc; node=Assign(a,e); id =0}} 
  | ADDR x=ID
    {{loc = $loc; node = Addr({loc = $loc; node = AccVar(x); id = 0}); id = 0}}
  | i=INT
    {{loc = $loc; node = ILiteral(i); id = 0}}
  | c=CHAR
    {{loc = $loc; node = CLiteral(c); id = 0}}
  | TRUE
    {{loc = $loc; node = BLiteral(true); id = 0}}
  | FALSE
    {{loc = $loc; node = BLiteral(false); id = 0}}
  | MINUS ex1=ex %prec UMINUS
    {{loc = $loc; node = UnaryOp(Neg,ex1); id = 0}}
  | NOT ex1=ex
    {{loc = $loc; node = UnaryOp(Not,ex1); id = 0}}
  | ex1=ex PLUS ex2=ex
    {{loc = $loc; node = BinaryOp(Add,ex1,ex2); id = 0}}
  | ex1=ex MINUS ex2=ex
    {{loc = $loc; node = BinaryOp(Sub,ex1,ex2); id = 0}}
  | ex1=ex TIMES ex2=ex
    {{loc = $loc; node = BinaryOp(Mult,ex1,ex2); id = 0}}
  | ex1=ex DIV ex2=ex
    {{loc = $loc; node = BinaryOp(Div,ex1,ex2); id = 0}}
  | ex1=ex MOD ex2=ex
    {{loc = $loc; node = BinaryOp(Mod,ex1,ex2); id = 0}}
  | ex1=ex EQ ex2=ex
    {{loc = $loc; node = BinaryOp(Equal,ex1,ex2); id = 0}}
  | ex1=ex NE ex2=ex
    {{loc = $loc; node = BinaryOp(Neq,ex1,ex2); id = 0}}
  | ex1=ex GREATER ex2=ex
    {{loc = $loc; node = BinaryOp(Greater,ex1,ex2); id = 0}}
  | ex1=ex LESS ex2=ex
    {{loc = $loc; node = BinaryOp(Greater,ex2,ex1); id = 0}}
  | ex1=ex GEQ ex2=ex
    {{loc = $loc; node = BinaryOp(Geq,ex1,ex2); id = 0}}
  | ex1=ex LEQ ex2=ex
    {{loc = $loc; node = BinaryOp(Geq,ex2,ex1); id = 0}}
  | ex1=ex AND ex2=ex
    {{loc = $loc; node = BinaryOp(And,ex1,ex2); id = 0}}
  | ex1=ex OR ex2=ex
    {{loc = $loc; node = BinaryOp(Or,ex1,ex2); id = 0}}
  | id=ID LPAR l=separated_list(COMMA ,ex ) RPAR
    {{loc=$loc; node=Call(id,l); id =0}}
  | LPAR e=ex RPAR
    {e}
  | error
    { raise (Syntax_error "errore in exp") }
;

