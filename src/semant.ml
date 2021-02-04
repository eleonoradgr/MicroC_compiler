open Ast
open Tast
open Symbol_table

type context = res Symbol_table.t
(* Since 4.11.0*)
let rec fold_left_map f acc y =
  match y with
  | [] -> (acc, [])
  | x::xs -> (let (acc', y) = f acc x in
              let (res, ys) = fold_left_map f acc' xs in
              (res, y :: ys))

(** Generate valid type for returned value of function
  @param t is Ast type representation
  @param l is the position in the lexing buffer
  @return res is the returned type
  @raise Semantic_error
*)
let rec typefun (t : typ) (l : position)=
  match t with
  | TypI      -> TInt
  | TypF      -> TFloat
  | TypB      -> TBool 
  | TypC      -> TChar
  | TypV      -> TVoid
  | TypA(x,y) -> Util.raise_semantic_error l "Function cannot return Array type"
  | TypP(x)   -> Util.raise_semantic_error l "Function cannot return Pointer type"
  
(** Generate valid type for variable
  @param t is the Ast type representation
  @param l is the position in the lexing buffer 
  @return res is the type
  @raise Semantic_error
*)
let rec typevar (t : typ) (l : position) =
  match t with
  | TypI      -> TInt
  | TypF      -> TFloat
  | TypB      -> TBool 
  | TypC      -> TChar
  | TypA(x,y) -> (match y with
                | Some(i) ->  TArray (( typevar x l), i)
                | None    ->  TArray (( typevar x l), 1))
  | TypP(x)   -> TPnt (typevar x l)
  | TypV      -> Util.raise_semantic_error l "Void is not a valid type for variable"

(** Generate a list of Texpr node for default initialization
  @param t type
  @param l position in the lexing buffer 
  @return list of Texpr node
  @raise Semantic_error
*)
let rec expr_init (t : res)  (l : position) =
  match t with
  | TInt        -> [{ty = TInt; node = (TILiteral 0); id = -1}]
  | TFloat      -> [{ty = TFloat; node = (TFLiteral 0.0); id = -1}]
  | TBool       -> [{ty = TBool; node = (TBLiteral false); id = -1}] 
  | TChar       -> [{ty = TChar; node = (TCLiteral (Char.chr(0))); id = -1}]
  | TArray(x,y) -> if y = 0 then [] else (expr_init x l)@(expr_init (TArray(x,(y-1))) l)
  | TPnt(x)     -> []
  | _       -> Util.raise_semantic_error l "Not a valid type for variable"

(** Add a new definition in the context
  @param id identifier
  @param t type
  @param gamma the context
  @param m message for semantic error
  @param l position in the lexing buffer
*)
let add_def (id : Ast.identifier) (t : res) (gamma : context) (m : string) (l : position)=
  try Symbol_table.add_entry id t gamma 
  with Symbol_table.DuplicateEntry -> Util.raise_semantic_error l m


(** Check if the expr node is evaluated to the requested type, and it is semantically correct
    This function is designed to evaluate expression in global environment which must have a constant value
  @param e expr
  @param gamma the context
  @param t the type expected for the expression, None if it's not requested a specific type.
  @return (gamma,texpr) the new context and the expression typed_node
  @raise Semantic_error
*)
let rec check_const_expr (e:expr) (gamma : context) (t : res option) =
  match e.node with
  | ILiteral(i)     -> (match t,i with
                        |None,_
                        |Some(TInt),_     
                        |Some(TFloat),_   -> let ty = Option.value t ~default:TInt in
                                            gamma,{ty = ty; node = TILiteral(i); id = e.id}
                        |Some(TPnt(_)),-1 -> gamma,{ty = (Option.get t); node = TILiteral(i); id = e.id} (*NULL pointer*)
                        |_->Util.raise_semantic_error e.loc (String.concat "" ["This expression has type Tast.TInt ";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] ))
  | FLiteral(f)     -> if(t = None || t = Some(TFloat))  then (gamma,{ty = TFloat; node = TFLiteral(f); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type Tast.TFloat ";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | CLiteral(c)     -> if(t = None || t = Some(TChar))  then (gamma,{ty = TChar; node = TCLiteral(c); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type Tast.TChar ";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | BLiteral(b)     -> if(t = None || t = Some(TBool))  then (gamma,{ty = TBool; node = TBLiteral(b); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type Tast.TBool ";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | Addr(a)         -> let rec acc ac = (match ac with
                                        |AccVar(i)    -> let ty =(try Symbol_table.lookup i gamma
                                                                  with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error a.loc "Variable not found" )in
                                                        {ty = ty; node = TAccVar(i); id = a.id}
                                        |AccIndex(b,i)-> (let (g,nex) =(check_const_expr i gamma (Some TInt) ) in
                                                          let tacc = acc b.node in 
                                                          match tacc.ty with
                                                          | TArray(x,i) -> {ty = x; node = TAccIndex(tacc,nex); id = a.id}
                                                          | _ -> Util.raise_semantic_error a.loc "Trying to access index of non arry")
                                        |_ -> Util.raise_semantic_error e.loc ("Initializer element is not constant")) in
                        let ty = acc a.node in
                        if(t = None || t = Some(TPnt ty.ty))  
                        then (gamma, {ty = (TPnt ty.ty); node = TAddr(ty); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type "; (show_res ty.ty); 
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | _ -> Util.raise_semantic_error e.loc ("Initializer element is not constant")


(** Check if the expr node is evaluated to the requested type, and it is semantically correct 
  @param e expr
  @param gamma the context
  @param t the type expected for the expression, None if it's not requested a specific type.
  @return (gamma,texpr) the new context and the expression typed_node
  @raise Semantic_error
*)
let rec check_expr  (e:expr) (gamma : context) (t : res option) =
  
  let rec type_of_access (a: access) (gamma : context) =
    (match a.node with
      | AccVar(i)         -> ( let ty =(try Symbol_table.lookup i gamma
                                        with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error a.loc "Variable not found" )in
                              {ty = ty; node = TAccVar(i); id = a.id} )
      | AccDeref(ex)      -> ( let x =( (*no pointer arithmetic, only simple access expected*)
                                      match ex.node with
                                      | Access(a) ->let y = type_of_access a gamma in
                                                    {ty = y.ty; node =TAccess(y); id = ex.id}
                                      | _         -> Util.raise_semantic_error a.loc "Dereferencing a non pointer") in
                                match x.ty with
                                | TPnt(t) -> {ty = t; node = TAccDeref(x); id = a.id}
                                | _       -> Util.raise_semantic_error a.loc "Dereferencing a non pointer" )
      | AccIndex(acc,idx) -> ( let (g,nex) =(check_expr idx gamma (Some TInt) ) in
                               let tacc = type_of_access acc gamma in 
                               match tacc.ty with
                                | TArray(x,i) -> {ty = x; node = TAccIndex(tacc,nex); id = a.id}
                                | _ -> Util.raise_semantic_error a.loc "Trying to access index of non arry") )

  in
  match e.node with
  | Access(a)       ->  let ty = type_of_access a gamma in
                        let check_array ty t = (match t with
                                                | Some(TArray(ty,_)) -> true                        
                                                | _ -> false) in 
                        if(t = None || t = Some(ty.ty) || check_array ty.ty t )
                        then (gamma, {ty = ty.ty; node = TAccess(ty); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type "; (show_res ty.ty); 
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | Assign(a,ex)    -> (let ty = type_of_access a gamma in
                        let (g,tex) = check_expr ex gamma (Some ty.ty) in
                        (g, {ty = ty.ty; node = TAssign(ty,tex); id = e.id}))
  | Addr(a)         ->  let ty = type_of_access a gamma  in
                        if(t = None || t = Some(TPnt ty.ty))  
                        then (gamma, {ty = (TPnt ty.ty); node = TAddr(ty); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type "; (show_res ty.ty); 
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | ILiteral(i)     -> check_const_expr e gamma t
  | FLiteral(f)     -> check_const_expr e gamma t
  | CLiteral(c)     -> check_const_expr e gamma t
  | BLiteral(b)     -> check_const_expr e gamma t
  | UnaryOp(u, ex)  -> ( match u with
                          |Inc
                          |Dec
                          |Neg ->  (match t with
                                    | None 
                                    | Some(TFloat) -> let (g, tex) = check_expr ex gamma (Some TFloat) in
                                                      (g,{ty = TFloat; node = TUnaryOp(u,tex); id = e.id})
                                    | Some(TInt)   -> let (g, tex) = check_expr ex gamma (Some TInt) in
                                                      (g,{ty = TInt; node = TUnaryOp(u,tex); id = e.id})
                                    |_ -> Util.raise_semantic_error ex.loc ("Only integer or float are valid operand")
                                     )
                          |Not ->  let (g, tex) = check_expr ex gamma (Some TBool) in
                                    (g,{ty = TBool; node = TUnaryOp(u,tex); id = e.id}))
  | BinaryOp(b, ex1, ex2) -> (match b with
                              | Add
                              | Sub
                              | Mult
                              | Div
                              | Mod -> (match t with
                                        | None 
                                        | Some(TFloat) -> let (g, tex) = check_expr ex1 gamma (Some TFloat)  in
                                                          let (g', tex') = check_expr ex2 g (Some TFloat) in
                                                          (g',{ty = TFloat; node = TBinaryOp(b,tex,tex'); id = e.id})
                                        | Some(TInt) -> let (g, tex) = check_expr ex1 gamma (Some TInt) in
                                                        let (g', tex') = check_expr ex2 g (Some TInt) in
                                                        (g',{ty = TInt; node = TBinaryOp(b,tex,tex'); id = e.id})
                                        |_ -> Util.raise_semantic_error e.loc ("Only integer or float are valid operand"))
                                      
                              | Equal 
                              | Neq 
                              | Less 
                              | Leq 
                              | Greater 
                              | Geq -> (try (let (g, tex) =check_expr ex1 gamma (Some TInt) in
                                              let (g', tex') = check_expr ex2 g (Some TInt) in
                                              (g',{ty = TBool; node = TBinaryOp(b,tex,tex'); id = e.id}))
                                        with Util.Semantic_error(m) -> (
                                          try (let (g, tex) = check_expr ex1 gamma (Some TFloat) in
                                              let (g', tex') = check_expr ex2 g (Some TFloat) in
                                              (g',{ty = TBool; node = TBinaryOp(b,tex,tex'); id = e.id}))
                                          with Util.Semantic_error(m) -> Util.raise_semantic_error e.loc ("Not valid types for binary operator")
                                        ))
                              | And 
                              | Or -> (let (g, tex) = check_expr ex1 gamma (Some TBool) in
                                        let (g', tex') = check_expr ex2 g (Some TBool) in
                                        (g',{ty = TBool; node = TBinaryOp(b,tex,tex'); id = e.id}))
                              | _ ->  Util.raise_semantic_error e.loc "Not valid operand")
  | Call(i,l)       -> (let f =( try Symbol_table.lookup i gamma 
                                 with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error e.loc "Function not defined" )in
                        match f with
                        | TFun(resl, res) -> (let rec checkparam listex reslist gamma ltex =
                                              match (listex, reslist) with
                                              |[],[] -> (gamma,ltex)
                                              |[],_
                                              |_,[] -> Util.raise_semantic_error e.loc "Invalid number of parameters"
                                              |(x::xs),(y::ys) -> let (g,tex) = check_expr x gamma (Some y) in
                                                                  checkparam xs ys g (ltex @ [tex])
                                              in
                                              let (g',tex') = checkparam l resl gamma [] in
                                              if (t = None || t = Some(res)) 
                                              then (g',{ty = res; node = TCall(i,tex'); id = e.id})
                                              else Util.raise_semantic_error e.loc (String.concat "" [" Expected "; (show_res (Option.get t));
                                                                                                  " but function returned "; (show_res res)] ))
                        | _ -> Util.raise_semantic_error e.loc (String.concat "" [i;" is not a function"]))      

(** Generate the initialization of a variable
  @param ty type of variable
  @param ex list of expression for initialization
  @param gamma the envirnoment
  @param global true if initialization for  aglobal variable, false ow
  @return the new typed expression list
*) 
let init_var (ty : res) (ex : expr list) (gamma : context) (global : bool)=
  let ce = if (global) then check_const_expr else check_expr  in
  match  ty with
  |TArray(ta,l) ->  let (new_gamma, texprl) = fold_left_map (fun x y -> (ce y x (Some ta))) gamma ex in
                    (*initialize an array of its declared length, eventually adding default values*)
                    if(l == 0 || l == List.length(texprl))
                    then texprl
                    else (Util.print_warning (List.hd ex).loc "Warning: initialized with different length array" ;
                          if(l > List.length(texprl))
                          then texprl@(expr_init (TArray(ta, (l-List.length texprl))) (List.hd ex).loc)
                          else List.map (List.nth texprl) (List.init l (fun x -> x)))
  |y            ->  let (new_gamma, texprl) = fold_left_map (fun x' y' -> (ce y' x' (Some y))) gamma ex in
                    texprl


(** Check if the stmt node is evaluated to the requested type, and it is semantically correct 
  @param s is the statement node
  @param gamma the context
  @param t the type expected for the statement, None if it's not requested a specific type.
  @param new_env true if the statemest must be evaluate in a new environment, false otherwise
  @return tstmt the new statement typed_node
  @raise Semantic_error  
*)                       
let rec check_stmt (s:stmt) (gamma : context) (t : res) (new_env : bool)= 
  match s.node with
  |If(e,s1,s2)  ->  let (gamma',tex) = check_expr e gamma (Some TBool)  in
                    let tstm1 = check_stmt s1 (Symbol_table.begin_block gamma) t false in
                    let tstm2 = check_stmt s2 (Symbol_table.begin_block gamma) t false in
                    { ty = t ; node = TIf( tex, tstm1, tstm2); id = s.id}
  |While(e,s1)  ->  let (gamma',tex) = check_expr e gamma (Some TBool)  in
                    let tstm1 = check_stmt s1 (Symbol_table.begin_block gamma) t false in
                    { ty = t ; node = TWhile(tex, tstm1); id = s.id}
  |Expr(e)      ->  let (gamma',tex) = check_expr e gamma None in
                    {ty = TVoid; node = TExpr(tex); id = s.id}
  |Return(e)    -> (match e with
                    | None    -> if t = TVoid then {ty = TVoid; node = TReturn(None); id = s.id} 
                                else Util.raise_semantic_error s.loc "Void function returned a non void value"
                    | Some(x) -> let (gamma',tex) = check_expr x gamma (Some t) in
                                {ty = t; node = TReturn(Some(tex)); id = s.id} ) 
  |Block(l)     -> (
                    let rec type_block (l : stmtordec list) (gamma : context) (tstmtsl : tstmtordec list) (ret : bool) =
                    match l with
                    | [] -> {ty = t; node = (TBlock tstmtsl); id = s.id}
                    | x::xs -> (match x.node with
                                |Dec(t,i,e) -> (let ty = typevar t x.loc in 
                                                let texprl = match e with
                                                              |Some(ex)  -> init_var ty ex gamma false 
                                                              |None      -> expr_init ty x.loc in
                                                let g = add_def i ty gamma "Variable already defined" x.loc in
                                                let d = {ty = ty; node = TDec( ty, i, texprl); id = x.id} in 
                                                if (ret) then type_block xs g (tstmtsl) ret else type_block xs g (tstmtsl @ [d]) ret)
                                |Stmt(s)    -> (let ts = check_stmt s gamma t true in
                                              let s1 = {ty = t; node = (TStmt ts); id = x.id} in
                                               if(ret) 
                                               then (Util.print_warning s.loc "Warning: Code in the block not reachable at this point"; 
                                                     type_block xs gamma (tstmtsl) ret ) 
                                               else( match s.node with
                                                      | Return(_) -> type_block xs gamma (tstmtsl @ [s1]) true
                                                      |_ -> type_block xs gamma (tstmtsl @ [s1]) ret
                                               )))
                    in
                    let gamma' = if(new_env) then  (Symbol_table.begin_block gamma) else gamma in
                    type_block l gamma' [] false)

(** Check if the topdecl is semantically correct 
  @param gamma the context
  @param topdecl global variable declaration of function declaration
  @return (gamma,ttopdec) the new context and declaration typed_node
  @raise Semantic_error  
*) 
let rec top_checking (gamma : context) (topdec : topdecl ) =
  match topdec.node with
  | Fundecl(y)     ->  (let ty = typefun y.typ topdec.loc in
                        let tyformals = List.map (fun (a,b)->((typevar a topdec.loc ), b))  y.formals in
                        let tynode = TFun((List.map (fun (a,b)-> a )  tyformals), ty) in
                        let g = add_def y.fname tynode gamma "Function already defined" topdec.loc in
                        let new_gamma = 
                          let rec add_param g fform =
                            match fform with
                            | [] -> g
                            | (t,i)::xs -> add_param (add_def i (typevar t topdec.loc) g "Parameter name already used" topdec.loc) xs 
                            in
                          add_param (Symbol_table.begin_block g) y.formals in
                        let  tbody = check_stmt y.body new_gamma ty false in
                        let tfun = {typ = ty; fname = y.fname; formals = tyformals; body = tbody} in
                        (g,{ ty = tynode; node = (TFundecl tfun); id = topdec.id}))
  | Vardec(t,i,e)   -> (let ty = (typevar t topdec.loc) in
                        let texprl = (match e with
                                    |Some(ex)   -> init_var ty ex gamma true
                                    |None       -> expr_init ty topdec.loc) in
                        let g = add_def i ty gamma "Variable already defined" topdec.loc in
                        (g,{ty = ty; node = TVardec( ty, i, texprl); id = topdec.id}))

(** Check main declaration 
  @param gamma the context
*) 
let check_main (gamma : context) =
  let t = try Symbol_table.lookup "main" gamma 
          with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error dummy_pos "Main function not defined" in
  match t with
  | TFun([],r)    -> if (r != TVoid && r != TInt) 
                    then Util.raise_semantic_error dummy_pos "Main function must return int or void"
  | TFun([par],r) -> if (r != TVoid && r != TInt) 
                    then Util.raise_semantic_error dummy_pos "Main function must return int or void"
                    else ( if ( par != TInt) 
                           then Util.raise_semantic_error dummy_pos "Main function must have at most one int parameter" )
  |_            -> Util.raise_semantic_error dummy_pos "Main is not a function"
  
  
(** Check if the program is semantically correct 
  @param Prog(topdecls) the AST of the program
  @return TProg (ttopd) the typed AST of the program
*) 
let check (Prog(topdecls)) =
  let gamma_init = 
    let g0 = Symbol_table.empty_table in
    let g1 = Symbol_table.add_entry "print" (TFun([TInt],TVoid)) g0 in
    let g2 = Symbol_table.add_entry "printfl" (TFun([TFloat],TVoid)) g1 in
    let g3 = Symbol_table.add_entry "printch" (TFun([TChar],TChar)) g2 in
    Symbol_table.add_entry "getint" (TFun([],TInt)) g3 in
  let (final_env, ttopd) = fold_left_map (fun x y -> (top_checking x y )) gamma_init topdecls in
  check_main final_env;
  TProg (ttopd)