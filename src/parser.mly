/*
* MicroC Parser specification
*/

%{
    open Ast
    open Util

    (* Define here your utility functions *)
    
    let counter = ref(0)
    let next_label () = incr counter; !counter

    let first xy = match xy with (x, y) -> x
    let second xy = match xy with (x, y) -> y

    let bulid_string s =
      let explode s =
        let char_to_exp c = {loc=dummy_pos ; node=CLiteral(c); id=next_label()} 
        in List.map char_to_exp (List.init (String.length s) ( String.get s))
      in List.append (explode s) [{loc=dummy_pos ; node=CLiteral('\x00'); id=next_label()}]

    let make_node l n =
      {loc=l ; node=n; id=next_label()}
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
  |  f=list(topdec) EOF       { Prog(f) }
  | error                     { raise (Syntax_error "error in program") }
;

topdec:
  | v=var e=init_var SEMICOLON               
    { make_node $loc (Vardec(first v, second v, e)) }
  | v=var LPAR par=separated_list(COMMA, var) RPAR b=block
    { make_node $loc (Fundecl {typ = first v; fname = second v; formals = par; body = b}) }
;

var:
  | t=type_def id=ID 
      { (t,id) }
  | t=type_def id=ID LBRACK RBRACK                   
      { (TypA(t,Some(0)), id) }
  | t=type_def id=ID LBRACK i=INT RBRACK        
      { (TypA(t,Some(i)), id) }
  | LPAR v=var RPAR                                           
      { v }
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
  |   { None }
  | ASS e=ex 
      { Some([e]) }
  | ASS LBRACE l=separated_list(COMMA, ex) RBRACE
      { Some(l) }
  | ASS s=STRING
      { Some( bulid_string s) }
;

block:
  | LBRACE b=list(inblock) RBRACE
    { make_node $loc (Block b) }
;

inblock:
  | v=var e=init_var SEMICOLON
    { make_node $loc (Dec(first v, second v, e)) }
  | s=stm
    { make_node $loc (Stmt s) }
;

stm:
  | IF LPAR e=ex RPAR b=stm  %prec THEN
    { let emptyb = make_node $loc (Block []) in
      make_node $loc (If(e,b,emptyb)) }
  | IF LPAR e=ex RPAR b1=stm ELSE b2=stm 
    { make_node $loc (If(e,b1,b2)) }
  | FOR LPAR a1=acc ASS e1=ex ";" e=ex ";" a2=acc ASS e2=ex RPAR s=stm
    { let new_block = make_node $loc (Block [( make_node $loc (Stmt(s)) );
                                            ( make_node $loc (Stmt(make_node $loc (Expr(make_node $loc (Assign(a2,e2)))))))])
      in
      make_node $loc (Block [ (make_node $loc (Stmt(make_node $loc (Expr(make_node $loc (Assign(a1,e1))))) ));
                              (make_node $loc (Stmt(make_node $loc (While(e,new_block)))))])
    }
  | FOR LPAR ";" e=ex ";" RPAR s=stm
    { make_node $loc (While(e,s)) }
  | WHILE LPAR e=ex RPAR s=stm
    { make_node $loc (While(e,s)) }
  | DO b=block WHILE LPAR e=ex RPAR SEMICOLON
    { make_node $loc (Block [( make_node $loc (Stmt(b)));
                            ( make_node $loc (Stmt(make_node $loc (While(e,b)))))]) }
  | e=ex SEMICOLON
    { make_node $loc (Expr e) } 
  | RETURN e = option( ex ) SEMICOLON
    { make_node $loc (Return e) }
  | b=block
    { b }
;

acc:
  | x=ID
    { make_node $loc (AccVar x) }
  | "*" a=acc %prec POINTER
    { make_node $loc (AccDeref(make_node $loc (Access a))) }
  | x=ID LBRACK ex2=ex RBRACK
    { make_node $loc (AccIndex(make_node $loc (AccVar x),ex2)) }
;

ex:
  | a=acc 
    { make_node $loc (Access a)}
  | a=assop %prec ASS 
    { a } 
  | ADDR x=ID
    { make_node $loc (Addr(make_node $loc (AccVar x))) }
  | i=INT
    { make_node $loc (ILiteral i) }
  | f=FLOAT
    { make_node $loc (FLiteral f) }
  | c=CHAR
    { make_node $loc (CLiteral c) }
  | TRUE
    { make_node $loc (BLiteral true) }
  | FALSE
    { make_node $loc (BLiteral false) }
  | MINUS ex1=ex %prec UMINUS
    { make_node $loc (UnaryOp(Neg,ex1)) }
  | NOT ex1=ex
    { make_node $loc (UnaryOp(Not,ex1)) }
  | ex1=ex b=binop ex2=ex
    { make_node $loc (BinaryOp(b,ex1,ex2)) }
  | ex1=ex LESS ex2=ex
    { make_node $loc (BinaryOp(Greater,ex2,ex1)) }
  | ex1=ex LEQ ex2=ex
    { make_node $loc (BinaryOp(Geq,ex2,ex1)) }
  | id=ID LPAR l=separated_list(COMMA ,ex ) RPAR
    { make_node $loc (Call(id,l)) }
  | LPAR e=ex RPAR
    {e}
  | error
    { raise (Syntax_error "errore in exp") }
;

assop:
  | a=acc ASS e=ex     
    { make_node $loc (Assign(a,e)) }
  | a=acc PLUSASS e=ex 
    { let new_exp = make_node $loc (BinaryOp(Add, (make_node $loc (Access a)), e)) in
     make_node $loc (Assign(a,new_exp)) }
  | a=acc MINUSASS e=ex
    { let new_exp = make_node $loc (BinaryOp(Sub, (make_node $loc (Access a)), e)) in
     make_node $loc (Assign(a,new_exp)) }
  | a=acc MULTASS e=ex
    { let new_exp = make_node $loc (BinaryOp(Mult, (make_node $loc (Access a)), e)) in
     make_node $loc (Assign(a,new_exp)) }
  | a=acc DIVASS e=ex
    { let new_exp = make_node $loc (BinaryOp(Div, (make_node $loc (Access a)), e)) in
     make_node $loc (Assign(a,new_exp)) }
  | a=acc MODASS e=ex   
    { let new_exp = make_node $loc (BinaryOp(Mod, (make_node $loc (Access a)), e)) in
     make_node $loc (Assign(a,new_exp)) }
  | INC a=acc | a=acc INC
    { let new_exp = make_node $loc (BinaryOp(Add, (make_node $loc (Access a)), (make_node $loc (ILiteral 1)))) in
      make_node $loc (Assign(a,new_exp)) }
  | DEC a=acc | a=acc DEC
    {let new_exp = make_node $loc (BinaryOp(Sub, (make_node $loc (Access a)), (make_node $loc (ILiteral 1)))) in
      make_node $loc (Assign(a,new_exp))}
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
