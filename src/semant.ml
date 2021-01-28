open Ast
open Tast
open Symbol_table

type context = res Symbol_table.t
let rec fold_left_map f acc y =
  match y with
  | [] -> (acc, [])
  | x::xs -> (let (acc', y) = f acc x in
              let (res, ys) = fold_left_map f acc' xs in
              (res, y :: ys))

let rec typefun (t : typ) l=
  match t with
  | TypI      -> TInt
  | TypF      -> TFloat
  | TypB      -> TBool 
  | TypC      -> TChar
  | TypV      -> TVoid
  | TypA(x,y) -> Util.raise_semantic_error l "Function cannot return Array type"
  | TypP(x)   -> Util.raise_semantic_error l "Function cannot return Pointer type"
  

let rec typevar (t : typ) l =
  match t with
  | TypI      -> TInt
  | TypF      -> TFloat
  | TypB      -> TBool 
  | TypC      -> TChar
  | TypA(x,y) -> (match y with
                | Some(i) ->  TArray (( typevar x l), i)
                | None    ->  TArray (( typevar x l), 1))
  | TypP(x)   -> TPnt (typevar x l)
  | TypV      -> Util.raise_semantic_error l "void is not  avalid type for variables"

let rec expr_init (t : typ)  l =
  match t with
  | TypI      -> [{ty = TInt; node = (TILiteral 0); id = -1}]
  | TypF      -> [{ty = TFloat; node = (TFLiteral 0.0); id = -1}]
  | TypB      -> [{ty = TBool; node = (TBLiteral false); id = -1}] 
  | TypC      -> [{ty = TChar; node = (TCLiteral '\x00'); id = -1}]
  | TypA(x,y) -> if y = Some(0) || y = None then [] else (expr_init x l)@(expr_init (TypA(x,Some((Option.get )y-1))) l)
  | TypP(x)   -> []
  | TypV      -> Util.raise_semantic_error l "void is not  avalid type for variables"

let add_def (id : Ast.identifier) (t : res) (gamma : context) (m : string)  l=
  try Symbol_table.add_entry id t gamma 
  with Symbol_table.DuplicateEntry -> Util.raise_semantic_error l m

 
(*
let rec type_of_expr (e:expr) (gamma : context) =
  let rec type_of_access a gamma =
    (match a.node with
      | AccVar(i)       -> (try Symbol_table.lookup i gamma
                            with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error a.loc "Variable not found")
      | AccDeref(ex)     -> (match (type_of_expr ex gamma) with
                            | TPnt(t) -> t
                            | _       -> Util.raise_semantic_error a.loc "Dereferencing a non pointer")
      | AccIndex(acc,idx) -> if ( (type_of_expr idx gamma) = TInt) 
                          then (match (type_of_access acc gamma) with
                                | TArray(x) -> x
                                | _ -> Util.raise_semantic_error a.loc "Trying to access index of non arry")
                          else Util.raise_semantic_error a.loc "Index must be an integer" )

  in
  match e.node with
  | Access(a)       -> type_of_access a gamma
  | Assign(a,ex)    -> (let x = type_of_expr ex gamma in
                        let y = type_of_access a gamma in
                        if (x = y)
                        then x
                        else Util.raise_semantic_error a.loc ("Type mismatching: trying to assign type  to ") )
  | Addr(a)         -> TPnt(type_of_access a gamma)
  | ILiteral(i)     -> TInt
  | FLiteral(f)     -> TFloat
  | CLiteral(c)     -> TChar
  | BLiteral(b)     -> TBool
  | UnaryOp(u, ex)  -> ( let x = type_of_expr ex gamma in
                        match u with
                        |Neg ->  if (x = TInt || x = TFloat) then x else Util.raise_semantic_error ex.loc ("Only integer or float can be nagated")
                        |Not ->  if (x = TBool) then TBool else Util.raise_semantic_error ex.loc ("Not available only for boolean expession"))
  | BinaryOp(b, ex1, ex2) -> (let x1 = type_of_expr ex1 gamma in
                              let x2 = type_of_expr ex2 gamma in
                              match b with
                              | Add
                              | Sub
                              | Mult
                              | Div
                              | Mod -> if (x1 = x2 && (x1=TInt || x1=TFloat)) then x1
                                      else (if (x1 = TInt && x2 = TFloat) || (x2 = TInt && x1 = TFloat) 
                                            then TFloat 
                                            else Util.raise_semantic_error e.loc "Not valid types for binary operator")
                              | Equal 
                              | Neq 
                              | Less 
                              | Leq 
                              | Greater 
                              | Geq -> if (x1 = x2 && (x1=TInt || x1=TFloat)) ||
                                          ((x1 = TInt && x2 = TFloat) || (x2 = TInt && x1 = TFloat))
                                      then TBool
                                      else Util.raise_semantic_error e.loc "Not valid types for binary operator"
                              | And 
                              | Or -> if (x1 = x2 && x1=TBool)
                                      then TBool
                                      else Util.raise_semantic_error e.loc "Not valid types for binary operator" 
                              | _ ->  Util.raise_semantic_error e.loc "Not valid operand")
  | Call(i,l)       -> (let f =( try Symbol_table.lookup i gamma 
                                  with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error e.loc "Function not defined" )in
                        match f with
                        | TFun(resl, res) -> (let rec checkparam listex reslist =
                                              match (listex, reslist) with
                                              |[],[] -> true
                                              |[],_
                                              |_,[] -> false
                                              |(x::xs),(y::ys) -> if (type_of_expr x gamma) = y
                                                                  then checkparam xs ys
                                                                  else  Util.raise_semantic_error e.loc "Invalid types of parameters"
                                              in
                                              if (checkparam l resl) 
                                              then res
                                              else Util.raise_semantic_error e.loc "Invalid number of parameters")
                        | _ -> Util.raise_semantic_error e.loc "Function not defined") *)

let rec check_expr  (e:expr) (gamma : context) (t : res option) =
  let rec type_of_access (a: access) gamma =
    (match a.node with
      | AccVar(i)       -> (let ty =(try Symbol_table.lookup i gamma
                                    with Symbol_table.NotFoundEntry ->  Util.raise_semantic_error a.loc "Variable not found" )in
                            {ty = ty; node = TAccVar(i); id = a.id} )
      | AccDeref(ex)     -> ( let x =( match ex.node with
                                      | Access(a) ->let y = type_of_access a gamma in
                                                    {ty = y.ty; node =TAccess(y); id = ex.id}
                                      | _       -> Util.raise_semantic_error a.loc "Dereferencing a non pointer") in
                            match x.ty with
                            | TPnt(t) -> {ty = t; node = TAccDeref(x); id = a.id}
                            | _       -> Util.raise_semantic_error a.loc "Dereferencing a non pointer" 
                            )
      | AccIndex(acc,idx) -> ( let (g,nex) =(check_expr idx gamma (Some TInt) ) in
                              let tacc = type_of_access acc gamma in 
                              match tacc.ty with
                                | TArray(x,i) -> {ty = x; node = TAccIndex(tacc,nex); id = a.id}
                                | _ -> Util.raise_semantic_error a.loc "Trying to access index of non arry"))

  in
  match e.node with
  | Access(a)       -> let ty = type_of_access a gamma in
                        let check_array ty t = (match t with
                                                | Some(TArray(x,y)) -> (match ty with
                                                                        |TArray(x,_) -> true
                                                                        |_ -> false)
                                                | _ -> false) in 
                        if(t = None || t = Some(ty.ty) || (t=Some(TFloat) && ty.ty =TInt) || check_array ty.ty t ) 
                        then (gamma, {ty = ty.ty; node = TAccess(ty); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type "; (show_res ty.ty); 
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | Assign(a,ex)    -> (let ty = type_of_access a gamma in
                        let (g,tex) = check_expr ex gamma (Some ty.ty) in
                        (g, {ty = ty.ty; node = TAssign(ty,tex); id = e.id}))
  | Addr(a)         -> let ty = type_of_access a gamma  in
                        if(t = None || t = Some(TPnt ty.ty))  then (gamma, {ty = (TPnt ty.ty); node = TAddr(ty); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type "; (show_res ty.ty); 
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | ILiteral(i)     ->  if(t = None || t = Some(TInt) || t = Some(TFloat))  then (let ty = Option.value t ~default:TInt in
                                                                                  gamma,{ty = ty; node = TILiteral(i); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type TInt";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | FLiteral(f)     -> if(t = None || t = Some(TFloat))  then (gamma,{ty = TFloat; node = TFLiteral(f); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type TFloat";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | CLiteral(c)     -> if(t = None || t = Some(TChar))  then (gamma,{ty = TChar; node = TCLiteral(c); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type TChar";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | BLiteral(b)     -> if(t = None || t = Some(TBool))  then (gamma,{ty = TBool; node = TBLiteral(b); id = e.id})
                        else Util.raise_semantic_error e.loc (String.concat "" ["This expression has type TBool";
                                                                            "but an expression was expeced of type "; (show_res (Option.get t))] )
  | UnaryOp(u, ex)  -> ( match u with
                          |Neg ->  (match t with
                                    | None 
                                    | Some(TFloat) -> let (g, tex) = check_expr ex gamma (Some TFloat) in
                                                      (g,{ty = TFloat; node = TUnaryOp(u,tex); id = e.id})
                                    | Some(TInt) -> let (g, tex) = check_expr ex gamma (Some TInt) in
                                                      (g,{ty = TInt; node = TUnaryOp(u,tex); id = e.id})
                                    |_ -> Util.raise_semantic_error ex.loc ("Only integer or float can be nagated")
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
                                        |_ -> Util.raise_semantic_error e.loc ("Only integer or float can be operators"))
                                      
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
                                        )
                                        (*match t with
                                        | None 
                                        | Some(TFloat) -> let tm = check_expr ex1 gamma (Some TFloat) type_map in
                                                          let tm1 = check_expr ex2 gamma (Some TFloat) tm in
                                                          Symbol_table.add_entry (string_of_int e.id) TBool tm1
                                        | Some(TInt) -> let tm = check_expr ex1 gamma (Some TInt) type_map in
                                                        let tm1 = check_expr ex2 gamma (Some TInt) tm in
                                                        Symbol_table.add_entry (string_of_int e.id) TBool tm1
                                        |_ -> Util.raise_semantic_error e.loc ("Not valid types for binary operator")*))
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
                                              else Util.raise_semantic_error e.loc "Invalid returned type of function")
                        | _ -> Util.raise_semantic_error e.loc "Function not defined")      

let rec check_stmt (s:stmt) (gamma : context) (t : res) = 
  match s.node with
  |If(e,s1,s2)  ->  let (gamma',tex) = check_expr e gamma (Some TBool)  in
                    let tstm1 = check_stmt s1 (Symbol_table.begin_block gamma) t in
                    let tstm2 = check_stmt s2 (Symbol_table.begin_block gamma) t in
                    { ty = t ; node = TIf( tex, tstm1, tstm2); id = s.id}
  |While(e,s1)  ->  let (gamma',tex) = check_expr e gamma (Some TBool)  in
                    let tstm1 = check_stmt s1 (Symbol_table.begin_block gamma) t in
                    { ty = t ; node = TWhile(tex, tstm1); id = s.id}
  |Expr(e)      -> let (gamma',tex) = check_expr e gamma None in
                    {ty = TVoid; node = TExpr(tex); id = s.id}
  |Return(e)    -> (match e with
                    | None    -> if t = TVoid then {ty = TVoid; node = TReturn(None); id = s.id} 
                                else Util.raise_semantic_error s.loc "Void function returned a non void value"
                    | Some(x) -> let (gamma',tex) = check_expr x gamma (Some t) in
                                {ty = t; node = TReturn(Some(tex)); id = s.id} ) 
  |Block(l)    -> (let rec type_block (l : stmtordec list) gamma tstmtsl ret =
                    match l with
                    | [] -> {ty = t; node = (TBlock tstmtsl); id = s.id}
                    | x::xs -> (match x.node with
                                |Dec(t,i,e) -> (let ty = typevar t x.loc in 
                                                match e with
                                                  |Some(ex)  -> (let ty' = match ty with
                                                                        |TArray(x,i) -> x
                                                                        |y         -> y
                                                                in
                                                                let (new_gamma, texprl) = fold_left_map (fun x y -> (check_expr y x (Some ty'))) gamma ex in
                                                                let g = add_def i ty gamma "Variable already defined" x.loc in
                                                                let d = {ty = ty; node = TDec( ty, i, texprl); id = x.id} in 
                                                                if (ret) then type_block xs g (tstmtsl) ret else type_block xs g (tstmtsl @ [d]) ret )  
                                                  |None -> let texprl = expr_init t x.loc in
                                                            let g = add_def i ty gamma "Variable already defined" x.loc in
                                                            let d = {ty = ty; node = TDec( ty, i, texprl); id = x.id} in
                                                            if (ret) then type_block xs g (tstmtsl) ret else type_block xs g (tstmtsl @ [d]) ret )
                                |Stmt(s)    -> (let ts = check_stmt s gamma t  in
                                              let s1 = {ty = t; node = (TStmt ts); id = x.id} in
                                               if(ret) then (Util.print_warning s.loc "Code in the block not reachable at this point"; type_block xs gamma (tstmtsl) ret ) 
                                               else( match s.node with
                                                      | Return(_) -> type_block xs gamma (tstmtsl @ [s1]) true
                                                      |_ -> type_block xs gamma (tstmtsl @ [s1]) ret
                                               )))
                    in
                    type_block l (Symbol_table.begin_block gamma) [] false)


let rec type_checking (gamma : context) (topdec : topdecl ) =
  match topdec.node with
  | Fundecl(y) -> ( let ty = typefun y.typ topdec.loc in
                    let tyformals = List.map (fun (a,b)->((typevar a topdec.loc ), b))  y.formals in
                    let tynode = TFun((List.map (fun (a,b)-> a )  tyformals), ty) in
                    let g = add_def y.fname tynode gamma "Function already defined" topdec.loc in
                    let new_gamma = 
                      let rec add_param g fform =
                        match fform with
                        | [] -> g
                        | (t,i)::xs -> add_param (add_def i (typevar t topdec.loc) g "parameter already used" topdec.loc) xs 
                        in
                      add_param (Symbol_table.begin_block g) y.formals in
                    let  tbody = check_stmt y.body new_gamma ty in
                    let tfun = {typ = ty; fname = y.fname; formals = tyformals; body = tbody} in
                    (g,{ ty = tynode; node = (TFundecl tfun); id = topdec.id}))
  | Vardec(t,i,e) -> ( let ty = (typevar t topdec.loc) in
                        match e with
                        |Some(ex)  -> ( let ty' = match  ty with
                                                |TArray(x,i) -> x
                                                |y         -> y
                                        in
                                        let (new_gamma, texprl) = fold_left_map (fun x y -> (check_expr y x (Some ty'))) gamma ex in
                                        let g = add_def i ty gamma "Variable already defined" topdec.loc in
                                        (g,{ty = ty; node = TVardec( ty, i, texprl); id = topdec.id}))
                        |None ->  let texprl = expr_init t topdec.loc in
                                  let g = add_def i ty gamma "Variable already defined" topdec.loc in
                                  (g,{ty = ty; node = TVardec( ty, i, texprl); id = topdec.id}) )

let check (Prog(topdecls)) =
  let gamma_init = 
    let g0 = Symbol_table.empty_table in
    let g1 = Symbol_table.add_entry "print" (TFun([TInt],TVoid)) g0 in
    let g2 = Symbol_table.add_entry "printfl" (TFun([TFloat],TVoid)) g1 in
    Symbol_table.add_entry "getint" (TFun([],TInt)) g2 in
  let (final_env, ttopd) = fold_left_map (fun x y -> (type_checking x y )) gamma_init topdecls in 
  TProg (ttopd)