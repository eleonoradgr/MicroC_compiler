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
  ; (Dec, TInt), (L.build_add (L.const_int int_t (-1)), "dec")
  ; (Dec, TFloat), (L.build_fadd (L.const_float float_t (-1.)), "dec")
]
(* A table mapping a binary operator in the LLVM function that implemets it and its name *)
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

(* Translate a res type into a LLVM IR one*)
let rec lltype_of_type = function 
  | TVoid       -> void_t  
  | TInt        -> int_t
  | TFloat      -> float_t
  | TBool       -> bool_t
  | TChar       -> char_t
  | TArray(t,i) -> pointer_t (lltype_of_type t)
  | TPnt(t)     -> pointer_t (lltype_of_type t)
  | _           -> failwith "Invalid type"
  
(* Translate a res type into a LLVM IR one*)
let init_default (t : res) = match t with
  | TInt        -> L.const_int int_t 0
  | TFloat      -> L.const_float float_t 0.
  | TBool       -> L.const_int bool_t 0
  | TChar       -> L.const_int char_t 0
  | _           -> failwith "Invalid type"

                          
(** Cast llvalue operands to the same type
  @param e1 llvalue of first operand
  @param e2 llvalue of second operand
  @param ex1t type of first
  @param ex2t type of second
  @param ibuilder is the builder used to generate instructions 
  @result (l1,l2),type the couple of llvalue with same type and the type
*)
let cast e1 e2  ex1t ex2t builder = match  (ex1t, ex2t) with
| (TInt, TFloat) -> (L.build_sitofp e1 float_t "tmp" builder, e2), TFloat
| (TFloat, TInt) -> (e1, L.build_sitofp e2 float_t "tmp" builder), TFloat
| _ -> (e1,e2), ex1t

(** Generate code for a global expression
  @param env is the environment mapping names to llvalues 
  @param te expression typed node  
*)
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
                            |_ -> failwith("Initializer element is not constant")) in
                      acc a.node
    | _ -> failwith("Initializer element is not constant")

(** Generate code for expression
  @param env is the environment mapping names to llvalues
  @param ibuilder is the builder used to generate instructions 
  @param te expression typed node  
*)
let rec codegen_expr  env ibuilder te=
  let rec codgen_ass  env ibuilder = function
                    | TAccVar(i)    -> (try Symbol_table.lookup i env
                                        with NotFoundEntry -> Option.get(L.lookup_global i llmodule))
                    | TAccDeref(x)  -> let p = codegen_expr env ibuilder x in
                                        L.build_gep p [| L.const_int int_t 0 |] "tmp" ibuilder
                    | TAccIndex(a,i)-> let v = codgen_ass env ibuilder a.node in
                                        let v = if(L.classify_type (L.element_type (L.type_of v))== Array) then v else L.build_load v "tmp" ibuilder in
                                        let i = codegen_expr env ibuilder i in
                                        if(L.classify_type (L.element_type (L.type_of v))== Array)then L.build_gep v [| L.const_int int_t 0 ; i |] "tmp" ibuilder 
                                        else L.build_gep v [| i |] "tmp" ibuilder in
  
  match te.node with
    | TAccess(a)    ->  (match te.ty with
                        |TArray(r,_)      -> (* Array as a parameter*)
                                            let x = codgen_ass env ibuilder a.node in
                                            if(L.classify_type (L.element_type (L.type_of x))== Array) then L.build_struct_gep x 0 "acc" ibuilder
                                            else  L.build_load x "tmp" ibuilder
                        |_ -> L.build_load (codgen_ass env ibuilder a.node  ) "acc" ibuilder )
    | TAssign(a,ex) ->  let ass = codgen_ass env ibuilder a.node in
                        let e = codegen_expr env ibuilder ex in
                        (match ex.node with
                        |TUnaryOp(Inc,_)
                        |TUnaryOp(Dec,_)  -> (* Post increment and decrement cases*)
                                            let e1 = L.build_load ass "acc" ibuilder in 
                                            ignore (L.build_store e ass ibuilder); e1
                        |_                ->ignore (L.build_store e ass ibuilder); e )
    | TAddr(a)      ->  codgen_ass env ibuilder a.node
    | TILiteral(i)  -> (match te.ty with
                        |TInt     -> L.const_int int_t i
                        |TFloat   -> let i1 = L.const_int int_t i in
                                     L.build_sitofp i1 float_t "tmp" ibuilder
                        |TPnt(r)  -> L.const_pointer_null (lltype_of_type te.ty)
                        |_        -> failwith("not valid type"))
    | TFLiteral(f)  ->  L.const_float float_t f
    | TCLiteral(c)  ->  L.const_int char_t (Char.code c)
    | TBLiteral(b)  ->  L.const_int bool_t (Bool.to_int b)
    | TUnaryOp(u,ex)        -> let e = codegen_expr env ibuilder ex in
                               let (llvm_operator, label) = List.assoc (u,te.ty) primitive_unoperators in
                               llvm_operator e label ibuilder            
    | TBinaryOp(b,ex1,ex2)  -> let e1 = codegen_expr env ibuilder ex1 in
                               let e2 = codegen_expr env ibuilder ex2 in  
                               let (e1ll, e2ll), t = cast e1 e2  ex1.ty ex2.ty ibuilder in
                               let (llvm_operator, label) = List.assoc (b,t) primitive_operators in
                               llvm_operator e1ll e2ll label ibuilder     
    | TCall("print", [e])   -> let ex = codegen_expr env ibuilder e in
                               let print = L.lookup_function "print" llmodule |> Option.get in
                               L.build_call print [| ex |] "call_print" ibuilder
    | TCall("printfl", [e]) -> let ex = codegen_expr env ibuilder e in
                               let printfl = L.lookup_function "printfl" llmodule |> Option.get in
                               L.build_call printfl [| ex |] "call_printfl" ibuilder
    | TCall("printch", [e]) -> let ex = codegen_expr env ibuilder e in
                               let printch = L.lookup_function "printch" llmodule |> Option.get in
                               L.build_call printch [| ex |] "call_printch" ibuilder
    | TCall("getint", [])   -> let getint = L.lookup_function "getint" llmodule |> Option.get in
                               L.build_call getint [| |] "call_getint" ibuilder
    | TCall(i,l)            -> let fdef = Symbol_table.lookup i env in
                               (*evaluation oeder of parameter is compiler dependent*)
                               (*let actuals = List.rev ( List.map(codegen_expr env ibuilder) (List.rev l)) in*)
                               let actuals = List.map(codegen_expr env ibuilder) l in
                               let result = (match te.ty with 
                                            |TVoid -> ""
                                            | _ -> i^"_result") in
                               L.build_call fdef (Array.of_list actuals) result ibuilder

(** Add a block terminator if not present in the block 
  @param ibuilder is the builder used to generate instructions
  @param f function that insert the instruction for terminating block
*)
let add_terminal_stmt ibuilder f =
    match L.block_terminator (L.insertion_block ibuilder) with
        Some _ -> ()
        | None -> ignore (f ibuilder)
        
(** Generate code for a statement
  @param current_fun is the llvalue of the function the expression belongs to
  @param env is the environment mapping names to llvalues 
  @param ibuilder is the builder used to generate instructions  
*)
let rec codegen_stmt current_fun env ibuilder = function
  | TIf (e, s1, s2) ->  let bcont = L.append_block llcontext "cont" current_fun in 
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
                                                      |_,TArray(r,len)  -> (
                                                                            let local_var = L.build_alloca (array_t (lltype_of_type r) (List.length e)) i ibuilder in
                                                                            let env' = Symbol_table.add_entry i local_var env in
                                                                            let ls = List.map (codegen_expr env ibuilder ) e in
                                                                            let test i e = (let addressi = L.build_gep local_var [| L.const_int int_t 0; L.const_int int_t i |] "tmp" ibuilder in
                                                                                              ignore(L.build_store e addressi ibuilder)) in
                                                                            List.iteri test ls;
                                                                            codegen_block current_fun env' ibuilder xs)
                                                      |[],TPnt(r)       -> let local_var = L.build_alloca (lltype_of_type t) i ibuilder in
                                                                            let env' = Symbol_table.add_entry i local_var env in
                                                                            let e' = L.const_pointer_null (lltype_of_type t) in
                                                                            ignore(L.build_store e' local_var ibuilder);
                                                                            codegen_block current_fun env' ibuilder xs
                                                      |[x],_            -> let local_var = L.build_alloca (lltype_of_type t) i ibuilder in
                                                                            let env' = Symbol_table.add_entry i local_var env in
                                                                            let e' = codegen_expr env' ibuilder x in
                                                                            ignore(L.build_store e' local_var ibuilder);
                                                                            codegen_block current_fun env' ibuilder xs
                                                      |_                 -> failwith("errore"))
                                    |TStmt(s)-> let b = codegen_stmt current_fun env ibuilder s.node in
                                                codegen_block current_fun env b xs) in
                        codegen_block current_fun (Symbol_table.begin_block env) ibuilder l)

(** Generate code for function declaration
  @param name of the function
  @param (formals,ret) type of formals and returned type 
  @param llmodule  
*)
let function_decl name (formals,ret) llmodule =
    let formal_types = Array.of_list (List.map (lltype_of_type) formals) in 
    let ftype = L.function_type (lltype_of_type ret) formal_types in
    L.define_function name ftype llmodule

(** Add the parameters to the local envirnment
  @param env is the environment mapping names to llvalues
  @param formals itype and name of parameters
  @param llformals llvalue parameters 
  @param builder is the builder used to generate instructions  
*)
let add_formals env formals llformals builder =
  let add_formal e (t,n) lt =
    L.set_value_name n lt;
    let local = L.build_alloca (lltype_of_type t) n builder in
    ignore (L.build_store lt local builder);
    Symbol_table.add_entry n local e in
List.fold_left2 add_formal env formals llformals

(** If the main has an int parameter it is substituted by a call to getint function
  @param env is the environment mapping names to llvalues
  @param formals type and name of parameters
  @param builder is the builder used to generate instructions  
*)
let build_param_main env formals builder =
  match formals with
  |[(t,n)] -> let a = Symbol_table.lookup n env in
              let getint = L.lookup_function "getint" llmodule |> Option.get in
              let e =  L.build_call getint [| |] "call_getint" builder in
              ignore (L.build_store e a builder)
  |[] -> ()
  |_ -> failwith("main can have just one input parameter")

(** Add a return statement to the function 
  @param builder is the builder used to generate instructions
  @param t type returned
*)
let add_terminal builder t =
    let f = match t with
            |TVoid -> L.build_ret_void
            |_     -> L.build_ret (init_default t) in
    add_terminal_stmt builder f

(** Generate code for a program and returns the generated llmodule
  @param topdelcs is the llvalue of the function the expression belongs to
  @param llmodule 
  @param env is the environment mapping names to llvalues  
*)
let rec codegen topdecls llmodule env = 
  match topdecls with
  | []   -> llmodule
  | x::xs -> match x.node with
              | TFundecl(y)    -> let test = List.map (fun (a,b)-> a) y.formals in
                                  let fdecl = function_decl y.fname ( test, y.typ) llmodule in
                                  let env = Symbol_table.add_entry y.fname fdecl env in
                                  let builder = L.builder_at_end llcontext (L.entry_block fdecl) in
                                  let env' = add_formals (Symbol_table.begin_block env) y.formals (Array.to_list (L.params fdecl)) builder in
                                  if(y.fname = "main") then build_param_main env' y.formals builder;
                                  let b = codegen_stmt fdecl env' builder (y.body).node in
                                  let _ = add_terminal b y.typ in
                                  codegen xs llmodule env

              | TVardec(t,i,e) ->  ((match e,t with
                                      |_,TArray(r,len) -> let ls = Array.of_list (List.map (codegen_expr_global env ) e) in
                                                          let x = L.const_array (lltype_of_type r) ls in
                                                          ignore(L.define_global i x llmodule)
                                      |[],TPnt(r)      -> let e' = L.const_pointer_null (lltype_of_type t) in
                                                          ignore(L.define_global i e' llmodule)
                                      |[x],_           -> let e' = codegen_expr_global env x in
                                                          ignore(L.define_global i e' llmodule)
                                      |_ -> failwith("error"));
                                    codegen xs llmodule env)

(* Declare in the current module the print and printf prototype *)  
let printf_declaration llvm_module =
  let print_t = L.function_type int_t [| int_t |] in
  let printfl_t = L.function_type float_t [| float_t |] in
  let printch_t = L.function_type char_t [| char_t |] in
  ignore(L.declare_function "print" print_t llvm_module);
  ignore(L.declare_function "printfl" printfl_t llvm_module);
  L.declare_function "printch" printch_t llvm_module

(* Declare in the current module the getint prototype *)  
let getint_declaration llvm_module =
  let getint_t = L.function_type int_t [| |] in
  L.declare_function "getint" getint_t llvm_module

let to_ir (TProg(ttopdecls)) =
  let env = Symbol_table.empty_table in
  ignore(printf_declaration llmodule);
  ignore(getint_declaration llmodule);
  codegen ttopdecls llmodule env
