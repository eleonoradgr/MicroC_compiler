open Ast
open Symbol_table

type res =
  | TVoid
  | TInt
  | TFloat
  | TBool
  | TChar
  | TArray of res
  | TPnt of res
  | TFun of res list * res 
  [@@deriving show]

type context = res Symbol_table.t

let rec typerep (t : typ) =
  match t with
  | TypI      -> TInt
  | TypF      -> TFloat
  | TypB      -> TBool 
  | TypC      -> TChar
  | TypA(x,y) -> TArray( typerep(x) )
  | TypP(x)   -> TPnt (typerep(x))
  | TypV      -> TVoid



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
                        | _ -> Util.raise_semantic_error e.loc "Function not defined")



let rec check_stmt (s:stmt) (gamma : context) (t : res)= 
  match s.node with
  |If(e,s1,s2)  -> if ((type_of_expr e gamma) = TBool)
                   then (let x1 = check_stmt s1 (Symbol_table.begin_block gamma) t in
                          let x2 = check_stmt s2 (Symbol_table.begin_block gamma) t in
                          if (x1 = x2) 
                          then x1
                          else Util.raise_semantic_error s.loc "Branches have different types" )
                   else Util.raise_semantic_error s.loc "Not a boolean guard"

  |While(e,s1)  -> if (type_of_expr e gamma) = TBool
                   then check_stmt s1 (Symbol_table.begin_block gamma) t
                   else Util.raise_semantic_error e.loc "Not a boolean guard"
  |Expr(e)      -> (match (type_of_expr e gamma) with _ -> true)
  |Return(e)    -> (match e with
                    | None    -> if t = TVoid then true else false
                    | Some(x) -> t = (type_of_expr x gamma)) 
  |Block(l)    -> (let rec type_block l gamma =
                    match l with
                    | [] -> true
                    | x::xs -> (match x.node with
                                |Dec(t,i,e) -> ( match e with
                                                  |Some(ex)  -> (let ty = match  typerep(t) with
                                                                        |TArray(x) -> x
                                                                        |y         -> y
                                                                in
                                                                if (List.for_all (fun x -> (type_of_expr x gamma)=ty) ex)
                                                                then type_block xs (Symbol_table.add_entry i (typerep(t)) gamma)
                                                                else Util.raise_semantic_error x.loc "Not valid init")  
                                                  |None -> type_block xs (Symbol_table.add_entry i (typerep(t)) gamma))
                                |Stmt(s)    -> if(check_stmt s gamma t)
                                               then type_block xs gamma
                                               else  Util.raise_semantic_error x.loc "Returned a not valid value" ) 
                    in
                    type_block l (Symbol_table.begin_block gamma))


let rec type_checking (topdecls : topdecl list) (gamma : context) =
  match topdecls with
  | [] -> ()
  | x::xs -> match x.node with
            | Fundecl(y) -> (
              let g = Symbol_table.add_entry y.fname  (TFun((List.map (fun (x,y)->typerep(x))  y.formals), typerep(y.typ))) gamma in
              let new_gamma = 
                let rec add_param g fform =
                  match fform with
                  | [] -> g
                  | (x,y)::xs -> add_param (Symbol_table.add_entry y (typerep(x)) g) xs 
                  in
                add_param (Symbol_table.begin_block g) y.formals
              in 
              if (check_stmt y.body new_gamma (typerep y.typ))
              then type_checking  xs g
              else Util.raise_semantic_error x.loc "Not valid function type"
            )
            | Vardec(t,i,e) -> ( match e with
                                  |Some(ex)  -> ( let ty = match  typerep(t) with
                                                          |TArray(x) -> x
                                                          |y         -> y
                                                  in
                                                  if (List.for_all (fun x -> (type_of_expr x gamma)=ty) ex)
                                                  then type_checking xs (Symbol_table.add_entry i (typerep(t)) gamma)
                                                  else Util.raise_semantic_error x.loc "Not valid init")  
                                  |None -> type_checking xs (Symbol_table.add_entry i (typerep(t)) gamma))

let check (Prog(topdecls)) = 
  let gamma_init = 
    let g0 = Symbol_table.empty_table in
    let g1 = Symbol_table.add_entry "print" (TFun([TInt],TVoid)) g0 in
    Symbol_table.add_entry "getint" (TFun([],TInt)) g1 in
  type_checking topdecls gamma_init