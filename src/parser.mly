/*
* MicroC Parser specification
*/

%{
    open Ast
    open Util

    (* Define here your utility functions *)

      let first xy = match xy with (x, y) -> x
      let second xy = match xy with (x, y) -> y

      let bulid_string s =
        let explode s =
          let char_to_exp c = {loc=dummy_pos ; node=CLiteral(c); id=0} 
          in List.map char_to_exp (List.init (String.length s) ( String.get s))
        in List.append (explode s) [{loc=dummy_pos ; node=CLiteral('\x00'); id=0}]
%}

/* Tokens declarations */

%token EOF
%token <int> INT 
%token <float> FLOAT
%token <char> CHAR
%token TRUE FALSE
%token <string> STRING
%token NULL VOID
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_CHAR
%token TYPE_BOOL
%token <string> ID
%token RETURN WHILE FOR IF ELSE DO
%token PLUS MINUS DIV MOD
%token TIMES "*"
%token EQ NEQ LESS GREATER LEQ GEQ
%token OR AND NOT
%token ASS PLUSASS MINUSASS MULTASS DIVASS MODASS INC DEC
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

%nonassoc THEN
%nonassoc ELSE
%right ASS PLUSASS MINUSASS MULTASS DIVASS MODASS
%left OR
%left AND
%left EQ NEQ
%left  LESS GREATER LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%left LPAR LBRACK
%nonassoc NOT
%nonassoc UMINUS
%nonassoc POINTER
/* %nonassoc ACC  */

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
  | v=var e=init_var SEMICOLON               
    { {loc= $loc ; node=Vardec(first v, second v, e); id=0} }
  | f=fund
    { {loc= $loc ; node=f; id=0} }
;

fund:
/*| t=types_fun  id=ID LPAR par=separated_list(COMMA , var) RPAR b= block
      { Fundecl({typ = t; fname = id ; formals = par; body = b}) }*/
  | v=var LPAR par=separated_list(COMMA, var) RPAR b=block
      { Fundecl({typ = first v; fname = second v; formals = par; body = b}) }
; 

var:
  | t=type_def id=ID 
      { (t,id) }
  | t=type_def id=ID LBRACK RBRACK                   
      { (TypA(t,Some(0)), id) }
  | t=type_def id=ID LBRACK i=INT RBRACK        
      {(TypA(t,Some(i)), id)}
  | LPAR v=var RPAR                                           
      {v}
;

type_def:
  | TYPE_INT
    { TypI }
  | TYPE_FLOAT
    { TypF }
  | TYPE_BOOL
    { TypB }
  | TYPE_CHAR
    { TypC }
  | VOID
    { TypV }
  | t=type_def TIMES
    { TypP(t) }
;

init_var:
  | { None }
  | ASS e=ex 
      {Some([e])}
  | ASS LBRACE l=separated_list(COMMA, ex) RBRACE
      {Some(l)}
  | ASS s=STRING
      {Some( bulid_string s) }
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
  | v=var e=init_var SEMICOLON
    {{loc = $loc; node = Dec(first v, second v, e); id = 0}}
  | s=stm
    {{loc = $loc; node = Stmt(s); id = 0}}
;

stm:
  | IF LPAR e=ex RPAR b=stm  %prec THEN
    {let emptyb = {loc = $loc; node = Block([]); id = 0 } in
    {loc = $loc; node = If(e,b,emptyb); id = 0 }}
  | IF LPAR e=ex RPAR b1=stm ELSE b2=stm 
    {{loc = $loc; node = If(e,b1,b2); id = 0 }}
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
  | DO b=block WHILE LPAR e=ex RPAR SEMICOLON
    {{loc = $loc; 
      node =Block([{loc=$loc;
                    node=Stmt(b);
                    id=0};
                    {loc=$loc;
                    node=Stmt({loc=$loc; node=While(e,b); id=0});
                    id=0}]); 
      id = 0 }}
  | id=ID LPAR l=separated_list(COMMA ,ex ) RPAR SEMICOLON
    {{loc = $loc; node = Expr({loc=$loc; node=Call(id,l); id =0}); id = 0}}
  | a=assop SEMICOLON
    {{loc = $loc; node = Expr(a); id = 0}} 
  | RETURN e = option( ex ) SEMICOLON
    {{loc = $loc; node = Return(e); id = 0 }}
  | b=block
    {b}
;

elsestm:
  | ELSE s=stm
    {s}
;
acc:
  | x=ID
    {{loc = $loc; node = AccVar(x); id = 0}}
  | "*" a=acc %prec POINTER
    {{loc = $loc; node = AccDeref({loc = $loc; node = Access(a); id = 0}); id = 0}}
  | x=ID LBRACK ex2=ex RBRACK
    {{loc = $loc; node = AccIndex({loc = $loc; node = AccVar(x); id = 0},ex2); id = 0}}
;
ex:
  | a=acc 
    {{loc=$loc; node=Access(a); id=0}}
  | a=assop %prec ASS 
    { a } 
  | ADDR x=ID
    {{loc = $loc; node = Addr({loc = $loc; node = AccVar(x); id = 0}); id = 0}}
  | i=INT
    {{loc = $loc; node = ILiteral(i); id = 0}}
  | f=FLOAT
    {{loc = $loc; node = FLiteral(f); id = 0}}
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
  | ex1=ex b=binop ex2=ex
    {{loc = $loc; node = BinaryOp(b,ex1,ex2); id = 0}}
  | ex1=ex LESS ex2=ex
    {{loc = $loc; node = BinaryOp(Greater,ex2,ex1); id = 0}}
  | ex1=ex LEQ ex2=ex
    {{loc = $loc; node = BinaryOp(Geq,ex2,ex1); id = 0}}
  | id=ID LPAR l=separated_list(COMMA ,ex ) RPAR
    {{loc=$loc; node=Call(id,l); id =0}}
  | LPAR e=ex RPAR
    {e}
  | error
    { raise (Syntax_error "errore in exp") }
;

assop:
  | a=acc ASS e=ex     
    { {loc=$loc; node=Assign(a,e); id =0} }
  | a=acc PLUSASS e=ex 
    { let new_exp = {loc=$loc; 
                    node=BinaryOp(Add,{loc=$loc; node=Access(a); id=0},e); 
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | a=acc MINUSASS e=ex
    { let new_exp = {loc=$loc;
                    node=BinaryOp(Sub,{loc=$loc; node=Access(a); id=0},e);
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | a=acc MULTASS e=ex
    { let new_exp = {loc=$loc;
                    node=BinaryOp(Mult,{loc=$loc; node=Access(a); id=0},e);
                    id=0}in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | a=acc DIVASS e=ex
    { let new_exp = {loc=$loc;
                    node=BinaryOp(Div,{loc=$loc; node=Access(a); id=0},e);
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | a=acc MODASS e=ex   
    { let new_exp = {loc=$loc;
                    node=BinaryOp(Mod,{loc=$loc; node=Access(a); id=0},e);
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | INC a=acc | a=acc INC
    {let new_exp = {loc=$loc;
                    node=BinaryOp(Add,{loc=$loc; node=Access(a); id=0},
                                      {loc=$loc; node=ILiteral(1); id=0});
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
  | DEC a=acc | a=acc DEC
    {let new_exp = {loc=$loc;
                    node=BinaryOp(Sub,{loc=$loc; node=Access(a); id=0},
                                      {loc=$loc; node=ILiteral(1); id=0});
                    id=0} in
     {loc=$loc; node=Assign(a,new_exp); id =0}}
;

%inline binop:
  | PLUS      { Add }
  | MINUS     { Sub }
  | TIMES     { Mult }
  | DIV       { Div }
  | MOD       { Mod }
  | EQ        { Equal }
  | NEQ       { Neq }
  | GREATER   { Greater }
  | GEQ       { Geq }
  | AND       { And }
  | OR        { Or }
;
