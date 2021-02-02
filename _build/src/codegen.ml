open Ast
open Tast
open Symbol_table
open Semant

(* A shorthand for referring to Llvm module *)
module L = Llvm

(* The LLVM global context *)
let llcontext = L.global_context ()

let llmodule = L.create_module llcontext "microc"

(* Some useful LLVM IR type to use in the code generation *)
let int_t = L.i32_type llcontext
let float_t = L.float_type llcontext
let bool_t = L.i1_type llcontext
let char_t = L.i8_type llcontext
let void_t = L.void_type llcontext
let array_t = L.array_type
let pointer_t = L.pointer_type

(* A table mapping a unary operator in the LLVM function that implemets it and its name *)
let primitive_unoperators = [
    (Neg, TInt), (L.build_neg, "neg")
  ; (Neg, TFloat), (L.build_fneg, "neg")
  ; (Not, TBool), (L.build_not, "not")
  ; (Inc, TInt), (L.build_add (L.const_int int_t 1), "inc")
  ; (Inc, TFloat), (L.build_fadd (L.const_float float_t 1.), "inc")
  ; (Dec, TInt), (L.build_sub (L.const_int int_t 1), "dec")
  ; (Dec, TFloat), (L.build_fsub (L.const_float float_t 1.), "dec")
]
let primitive_operators = 
  [ (Add, TInt), (L.build_add, "add")
  ; (Add, TFloat), (L.build_fadd, "add") 
  ; (Sub,TInt), (L.build_sub, "sub")
  ; (Sub,TFloat), (L.build_fsub, "sub")
  ; (Mult,TInt),(L.build_mul, "mul")
  ; (Mult,TFloat),(L.build_fmul, "mul")
  ; (Div,TInt), (L.build_sdiv, "div")
  ; (Div,TFloat), (L.build_fdiv, "div")
  ; (Mod,TInt), (L.build_srem, "mod")
  ; (Mod,TFloat), (L.build_frem, "mod")
  ; (Equal, TInt), (L.build_icmp L.Icmp.Eq, "equal")
  ; (Equal, TFloat), (L.build_fcmp L.Fcmp.Oeq, "equal")
  ; (Neq, TInt), (L.build_icmp L.Icmp.Ne, "notequal")
  ; (Neq, TFloat), (L.build_fcmp L.Fcmp.One, "notequal")
  ; (Less, TInt), (L.build_icmp L.Icmp.Slt, "less")
  ; (Less, TFloat), (L.build_fcmp L.Fcmp.Olt, "less")
  ; (Leq, TInt), (L.build_icmp L.Icmp.Sle, "less_equal")
  ; (Leq, TFloat), (L.build_fcmp L.Fcmp.Ole, "less_equal")
  ; (Greater, TInt), (L.build_icmp L.Icmp.Sgt, "greater")
  ; (Greater, TFloat), (L.build_fcmp L.Fcmp.Ogt, "greater")
  ; (Geq, TInt), (L.build_icmp L.Icmp.Sge, "greater_equal")
  ; (Geq, TFloat), (L.build_fcmp L.Fcmp.Oge, "greater_equal")
  ; (And, TBool), (L.build_and, "and")
  ; (Or, TBool), (L.build_or, "or")
  ]

(* Translate a FUN type into a LLVM IR one*)
let rec lltype_of_type = function 
  | TVoid       -> void_t  
  | TInt        -> int_t
  | TFloat      -> float_t
  | TBool       -> bool_t
  | TChar       -> char_t
  | TArray(t,i) -> pointer_t (lltype_of_type t)
  | TPnt(t)     -> pointer_t (lltype_of_type t)
  | _           -> failwith "Invalid type"
  

let rec lltype_of_typeres = function 
  | TVoid     -> void_t
  | TInt      -> int_t
  | TFloat    -> float_t
  | TBool     -> bool_t
  | TChar     -> char_t
  | _           -> failwith "Invalid type 1"

let init_default t = match t with
  | TInt        -> L.const_int int_t 0
  | TFloat      -> L.const_float float_t 0.
  | TBool       -> L.const_int bool_t 0
  | TChar       -> L.const_int char_t 0
  | _           -> failwith "Invalid type 2"

                          
(** Generate code for expressions
  @param current_fun is the llvalue of the function the expression belongs to
  @param env is the environment mapping names to llvalues 
  @param ibuilder is the builder used to generate instructions  
*)
let cast e1 e2  ex1t ex2t builder = match  (ex1t, ex2t) with
| (TInt, TFloat) -> (L.build_sitofp e1 float_t "tmp" builder, e2), TFloat
| (TFloat, TInt) -> (e1, L.build_sitofp e2 float_t "tmp" builder), TFloat
| _ -> (e1,e2), ex1t

let rec codegen_expr_global env te = match te.node with
    | TILiteral(i) -> L.const_int int_t i
    | TFLiteral(f) -> L.const_float float_t f
    | TCLiteral(c) -> L.const_int char_t (Char.code c)
    | TBLiteral(b) -> L.const_int bool_t (Bool.to_int b)
    | TAddr(a)     -> let rec acc ac = (match ac with
                            |TAccVar(i) -> Option.get(L.lookup_global i llmodule)
                            |TAccIndex(b,i)-> let x = codegen_expr_global env i in
                                              let arr = acc b.node in
                                              L.const_gep arr [|L.const_int int_t 0 ; x |];
                            |_ -> failwith("initializer element is not constant")) in
                      acc a.node
    | _ -> failwith("initializer element is not constant")


let rec codegen_expr  env ibuilder te=
  let rec codgen_ass  env ibuilder = function
                    | TAccVar(i)    -> (try Symbol_table.lookup i env
                                        with NotFoundEntry -> let p = Option.get(L.lookup_global i llmodule) in
                                        (*Printf.printf "p : %s\n"(L.string_of_lltype (L.type_of p));*)
                                        p)
                    | TAccDeref(x)  -> let p = codegen_expr env ibuilder x in
                                        (*Printf.printf "acc deref : %s\n"(L.string_of_lltype (L.type_of p));*)
                                        L.build_gep p [| L.const_int int_t 0 |] "tmp" ibuilder
                    | TAccIndex(a,i)-> let v = codgen_ass env ibuilder a.node in
                                        let v = if(L.classify_type (L.element_type (L.type_of v))== Array) then v else L.build_load v "tmp" ibuilder in
                                        let i = codegen_expr env ibuilder i in
                                        (*Printf.printf "ass : %s\n"(L.string_of_lltype (L.type_of v));*)
                                        if(L.classify_type (L.element_type (L.type_of v))== Array)then L.build_gep v [| L.const_int int_t 0 ; i |] "tmp" ibuilder 
                                        else L.build_gep v [| i |] "tmp" ibuilder in
  let rec codgen_acc  env ibuilder = function
                    | TAccVar(i)    -> (try L.build_load (Symbol_table.lookup i env) i ibuilder
                                      with NotFoundEntry -> let p = Option.get(L.lookup_global i llmodule) in
                                        let p1 =  L.build_gep p [| L.const_int int_t 0 |] "acc" ibuilder in
                                        (*Printf.printf "p1 : %s\n"(L.string_of_lltype (L.type_of p));*)
                                        L.build_load p i ibuilder )
                    | TAccDeref(x)  -> let p = codegen_expr env ibuilder x in
                                      (*Printf.printf "acc deref : %s\n"(L.string_of_lltype (L.type_of p));*)
                                      L.build_load (L.build_gep p [| L.const_int int_t 0 |] "tmp" ibuilder) "tmp" ibuilder
                    | TAccIndex(a,i)-> let v = codgen_ass env ibuilder a.node in
                                      (*Printf.printf "Nell'acc ind prima : %s\n"(L.string_of_lltype (L.type_of v));*)
                                      let v = if(L.classify_type (L.element_type (L.type_of v))== Array) then v else L.build_load v "tmp" ibuilder in
                                      let i = codegen_expr env ibuilder i in
                                      (*Printf.printf "Nell'acc ind dopo : %s\n"(L.string_of_lltype (L.type_of v));*)
                                      if(L.classify_type (L.element_type (L.type_of v))== Array)then  L.build_load (L.build_gep v [| L.const_int int_t 0 ; i |] "tmp" ibuilder) "acc" ibuilder
                                       else  L.build_load (L.build_gep v [| i |] "tmp" ibuilder) "acc" ibuilder
                                      in                                      
  
  match te.node with
    | TAccess(a)  -> (match te.ty with
                      |TArray(r,_) -> (*ignore(Printf.printf("entro qui\n"));*)
                                      let x = codgen_ass env ibuilder a.node in
                                      if(L.classify_type (L.element_type (L.type_of x))== Array) then L.build_struct_gep x 0 "acc" ibuilder
                                      else  L.build_load x "tmp" ibuilder
                      |_ -> codgen_acc env ibuilder a.node )
    | TAssign(a,ex)-> let ass = codgen_ass env ibuilder a.node in
                      let e = codegen_expr env ibuilder ex in
                      (match ex.node with
                      |TUnaryOp(Inc,_)
                      |TUnaryOp(Dec,_)-> let e1 = codgen_acc env ibuilder a.node in 
                                          ignore(L.build_store e ass ibuilder); e1
                      |_ ->ignore (L.build_store e ass ibuilder); e)
    | TAddr(a)     -> codgen_ass env ibuilder a.node
                      (*L.build_gep p [| L.const_int int_t 0 |] "tmp" ibuilder*)
    | TILiteral(i) -> (match te.ty with
                      |TInt -> L.const_int int_t i
                      |TFloat -> let i1 = L.const_int int_t i in
                                  L.build_sitofp i1 float_t "tmp" ibuilder
                      |TPnt(r) -> L.const_pointer_null (lltype_of_type te.ty))
    | TFLiteral(f) -> L.const_float float_t f
    | TCLiteral(c) -> L.const_int char_t (Char.code c)
    | TBLiteral(b) -> L.const_int bool_t (Bool.to_int b)
    | TUnaryOp(u,ex)       -> let e = codegen_expr env ibuilder ex in
                            let (llvm_operator, label) = List.assoc (u,te.ty) primitive_unoperators in
                            llvm_operator e label ibuilder                  
    | TBinaryOp(b,ex1,ex2) ->  let e1 = codegen_expr env ibuilder ex1 in
                              let e2 = codegen_expr env ibuilder ex2 in  
                              let (e1ll, e2ll), t = cast e1 e2  ex1.ty ex2.ty ibuilder in
                              let (llvm_operator, label) = List.assoc (b,t) primitive_operators in
                              llvm_operator e1ll e2ll label ibuilder     
    | TCall("print", [e]) -> let ex = codegen_expr env ibuilder e in
                              let print = L.lookup_function "print" llmodule |> Option.get in
                              L.build_call print [| ex |] "call_print" ibuilder
    | TCall("printfl", [e]) -> let ex = codegen_expr env ibuilder e in
                               let str =  L.build_global_stringptr "%f\n" "fmt" ibuilder in
                               let print = L.lookup_function "printf" llmodule |> Option.get in
                              L.build_call print [| str ; ex |] "printf" ibuilder
    | TCall(i,l) -> let fdef = Symbol_table.lookup i env in
                    let actuals = List.rev ( List.map(codegen_expr env ibuilder) (List.rev l)) in
                    let result = (match te.ty with 
                                  |TVoid -> ""
                                  | _ -> i^"_result") in
                    L.build_call fdef (Array.of_list actuals) result ibuilder
(**
  Generate the code for the main expression e
  @param llmodule represents the LLVM module we are generating
  @param env an environment mapping function names to their translation
  @param e the expression we are translating

let codegen_main llmodule env e = 
  let maindef = L.define_function main_name main_type llmodule in  
  let ibuilder = L.builder_at_end llcontext (L.entry_block maindef) in
  let last_instr = codegen_expr maindef env ibuilder e in
  let str = Llvm.build_global_string "%d\n" "str_constr" ibuilder in
  let printf = L.lookup_function "printf" llmodule |> Option.get in
  let zero = Llvm.const_int (Llvm.i64_type llcontext) 0 in 
  let pstr = Llvm.build_gep str [|zero; zero|] "gep" ibuilder in 
  let _ = Llvm.build_call printf [|pstr; last_instr|] "call_printf" ibuilder in   
  let _ = L.build_ret_void ibuilder in 
  ()*)
let add_terminal_stmt builder f =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
        | None -> ignore (f builder)
        

let rec codegen_stmt current_fun env ibuilder = function
  | TIf (e, s1, s2) -> let bcont = L.append_block llcontext "cont" current_fun in 
                      let bthen = L.append_block llcontext "then" current_fun in 
                      let belse = L.append_block llcontext "else" current_fun in 
                      let te1 = codegen_expr env ibuilder e in 
                      add_terminal_stmt (codegen_stmt current_fun env (L.builder_at_end llcontext bthen) s1.node) (L.build_br bcont);
                      add_terminal_stmt (codegen_stmt current_fun env (L.builder_at_end llcontext belse) s2.node) (L.build_br bcont);
                      ignore(L.build_cond_br te1 bthen belse ibuilder);
                      L.builder_at_end llcontext bcont
  | TWhile (e, s1)  ->  let bwcond = L.append_block llcontext "cond" current_fun in
                        ignore(L.build_br bwcond ibuilder);
                        let bwbody = L.append_block llcontext "body" current_fun in
                        add_terminal_stmt (codegen_stmt current_fun env (L.builder_at_end llcontext bwbody) s1.node) (L.build_br bwcond);
                        let predb = L.builder_at_end llcontext bwcond in
                        let te1 = codegen_expr env predb e in
                        let bwcont = L.append_block llcontext "cont" current_fun in
                        ignore (L.build_cond_br te1 bwbody bwcont predb);
                        L.builder_at_end llcontext bwcont
  | TExpr (e)       ->  let _ = codegen_expr env ibuilder e in
                        ibuilder
  | TReturn(e)      ->  (match e with
                        |None -> ignore(L.build_ret_void ibuilder); ibuilder
                        |Some(x) -> ignore( L.build_ret (codegen_expr env ibuilder x) ibuilder ); ibuilder)
  | TBlock(l)       ->  (let rec codegen_block current_fun env ibuilder = function
                        | [] -> ibuilder
                        | x::xs -> (match x.node with
                                    |TDec(t,i,e) -> (match e,t with
                                                      |_,TArray(r,len) -> (let local_var = L.build_alloca (array_t (lltype_of_type r) len) i ibuilder in
                                                                            (*Printf.printf "local_var : %s\n"(L.string_of_lltype (L.element_type (L.type_of local_var)));*)
                                                                            let env' = Symbol_table.add_entry i local_var env in
                                                                            let ls = List.map (codegen_expr env ibuilder ) e in
                                                                            let test i e = (let addressi = L.build_gep local_var [| L.const_int int_t 0; L.const_int int_t i |] "tmp" ibuilder in
                                                                                              ignore(L.build_store e addressi ibuilder)) in
                                                                            List.iteri test ls;
                                                                            codegen_block current_fun env' ibuilder xs)
                                                      |[],TPnt(r)  -> let local_var = L.build_alloca (lltype_of_type t) i ibuilder in
                                                                      (*Printf.printf "local_var : %s\n"(L.string_of_lltype (L.type_of local_var));*)
                                                                      let env' = Symbol_table.add_entry i local_var env in
                                                                      let e' = L.const_pointer_null (lltype_of_type t) in
                                                                      (*Printf.printf "e : %s\n"(L.string_of_lltype (L.type_of e'));*)
                                                                      ignore(L.build_store e' local_var ibuilder);
                                                                      codegen_block current_fun env' ibuilder xs
                                                      |[x],_ -> let local_var = L.build_alloca (lltype_of_type t) i ibuilder in
                                                                let env' = Symbol_table.add_entry i local_var env in
                                                                let e' = codegen_expr env' ibuilder x in
                                                                ignore(L.build_store e' local_var ibuilder);
                                                                codegen_block current_fun env' ibuilder xs
                                                      |_ -> failwith("errore"))
                                                    (*let local_var = L.build_alloca (lltype_of_type t) i ibuilder in
                                                    let env' = Symbol_table.add_entry i local_var env in
                                                    ignore(match e with
                                                              |[x] -> let e' = codegen_expr env' ibuilder x in
                                                                      ignore(L.build_store e' local_var ibuilder)
                                                              |_ -> (match t with
                                                                      | TArray(r,len) -> (let ls = List.map (codegen_expr env ibuilder ) e in
                                                                                          let test i e = (let addressi = L.build_gep local_var [| L.const_int int_t 0; L.const_int int_t i |] "tmp" ibuilder in
                                                                                                            ignore(L.build_store e addressi ibuilder)) in
                                                                                          List.iteri test ls
                                                                                          (*let test = L.const_array (array_t (lltype_of_type r) len) ls in
                                                                                          L.build_pointercast test (L.pointer_type (lltype_of_type r)) "tmp" ibuilder*))
                                                                      |_ -> failwith("errore")) );
                                                    codegen_block current_fun env' ibuilder xs*)
                                    |TStmt(s)-> let b = codegen_stmt current_fun env ibuilder s.node in
                                                codegen_block current_fun env b xs) in
                        codegen_block current_fun env ibuilder l)


let function_decl name (formals,ret) llmodule =
    let formal_types = Array.of_list (List.map (lltype_of_type) formals) in 
    let ftype = L.function_type (lltype_of_type ret) formal_types in
    L.define_function name ftype llmodule


let add_formals env formals llformals builder =
  let add_formal e (t,n) lt =
    L.set_value_name n lt;
    let local = L.build_alloca (lltype_of_type t) n builder in
    ignore (L.build_store lt local builder);
    (*Printf.printf "parametro :%s\n"(L.string_of_lltype (L.type_of local));*)
    Symbol_table.add_entry n local e in
List.fold_left2 add_formal env formals llformals

let build_param_main env formals builder =
  (*Printf.printf "ci entro non matchano gli argo : %d \n" (List.length formals);*)
  match formals with
  |[(t,n)] -> let a = Symbol_table.lookup n env in
              let getint = L.lookup_function "getint" llmodule |> Option.get in
              let e =  L.build_call getint [| |] "call_getint" builder in
              (*Printf.printf "chiamo get int : \n";*)
              ignore (L.build_store e a builder)
  |[] -> ()
  |_ -> failwith("main can have just one input parameter")

let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> match f with
              |TVoid -> ignore(L.build_ret_void builder)
              |_ -> ignore (L.build_ret (init_default f) builder)


(* Generate code for a program and returns the generated llmodule *)  
let rec codegen topdecls llmodule env = 
  match topdecls with
  | []   -> llmodule
  | x::xs -> match x.node with
              | TFundecl(y)    -> let test = List.map (fun (a,b)-> a) y.formals in
                                  let fdecl = function_decl y.fname ( test, y.typ) llmodule in
                                  let env = Symbol_table.add_entry y.fname fdecl env in
                                  let builder = L.builder_at_end llcontext (L.entry_block fdecl) in
                                  let env' = add_formals (Symbol_table.begin_block env) y.formals (Array.to_list (L.params fdecl)) builder in
                                  (*Printf.printf "la funzione si chiama : %s %b\n" y.fname (y.fname == "main/");*)
                                  if(y.fname = "main") then build_param_main env' y.formals builder;
                                  let b = codegen_stmt fdecl env' builder (y.body).node in
                                  let _ = add_terminal b y.typ in
                                  codegen xs llmodule env

              | TVardec(t,i,e) ->  (let env' = (match e,t with
                                                |_,TArray(r,len) -> ( let ls = Array.of_list (List.map (codegen_expr_global env ) e) in
                                                                      let x = L.const_array (lltype_of_type r) ls in
                                                                      (*Printf.printf "x : %s\n"(L.string_of_lltype (L.type_of x));*)
                                                                      ignore(L.define_global i x llmodule);
                                                                      env)
                                                |[],TPnt(r)      -> let e' = L.const_pointer_null (lltype_of_type t) in
                                                                    Symbol_table.add_entry i (L.define_global i e' llmodule) env
                                                |[x],_ -> let e' = codegen_expr_global env x in
                                                        Symbol_table.add_entry i (L.define_global i e' llmodule) env
                                                |_ -> failwith("errore")) in
                                    codegen xs llmodule env')

(* Declare in the current module the printf prototype *)  
let printf_declaration llvm_module =
  let print_t = L.function_type int_t [| int_t |] in
  let printfl_t = L.var_arg_function_type int_t [| L.pointer_type char_t |] in
  ignore(L.declare_function "print" print_t llvm_module);
  L.declare_function "printf" printfl_t llvm_module

let getint_declaration llvm_module =
  let getint_t = L.function_type int_t [| |] in
  L.declare_function "getint" getint_t llvm_module

let to_ir (TProg(ttopdecls)) =
  let env = Symbol_table.empty_table in
  ignore(printf_declaration llmodule);
  ignore(getint_declaration llmodule);
  codegen ttopdecls llmodule env
