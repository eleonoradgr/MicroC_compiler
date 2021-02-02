open Ast

type res =
  | TVoid
  | TInt
  | TFloat
  | TBool
  | TChar
  | TArray of res * int
  | TPnt of res
  | TFun of res list * res 
  [@@deriving show]

type 'a typed_node = {ty: res; node : 'a; id : int }[@@deriving show]


and texpr =  texpr_node typed_node
and texpr_node =
  | TAccess of taccess                 (* x    or  *p    or  a[e]     *)
  | TAssign of taccess * texpr          (* x=e  or  *p=e  or  a[e]=e   *)
  | TAddr of taccess                   (* &x   or  &*p   or  &a[e]    *)
  | TILiteral of int                  (* Integer literal             *)
  | TFLiteral of float                (* Float literal               *)
  | TCLiteral of char                 (* Char literal                *)
  | TBLiteral of bool                 (* Bool literal                *)
  | TUnaryOp of uop * texpr            (* Unary primitive operator    *)
  | TBinaryOp of binop * texpr * texpr  (* Binary primitive operator   *)
  | TCall of identifier * texpr list   (* Function call f(...)        *)
[@@deriving show]

and taccess = taccess_node typed_node
and taccess_node =
  | TAccVar of identifier             (* Variable access        x    *)
  | TAccDeref of texpr                 (* Pointer dereferencing  *p   *)
  | TAccIndex of taccess * texpr        (* Array indexing         a[e] *)
[@@deriving show]

and tstmt = tstmt_node typed_node
and tstmt_node =
  | TIf of texpr * tstmt * tstmt         (* Conditional                 *)
  | TWhile of texpr * tstmt             (* While loop                  *)
  | TExpr of texpr                     (* Expression statement   e;   *)
  | TReturn of texpr option            (* Return statement            *)
  | TBlock of tstmtordec list          (* Block: grouping and scope   *)
[@@deriving show]

and tstmtordec = tstmtordec_node typed_node
and tstmtordec_node =
  | TDec of res * identifier * texpr list  (* Local variable declaration  *)
  | TStmt of tstmt                                (* A statement                 *)
[@@deriving show]

type tfun_decl = {
  typ : res;
  fname : string;
  formals : (res*identifier) list;
  body : tstmt;
}[@@deriving show]

type ttopdecl = ttopdecl_node typed_node
and ttopdecl_node =
  | TFundecl of tfun_decl
  | TVardec of res * identifier * texpr list
[@@deriving show]

type tprogram = TProg of ttopdecl list [@@deriving show]