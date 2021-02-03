exception DuplicateEntry
exception NotFoundEntry

module StringMap = Map.Make(String)

type 'a t = ('a StringMap.t) list (* TODO: this is a dummy definition *)

let empty_table = [ StringMap.empty ]

let begin_block table = (StringMap.empty)::table

let end_block table = match table with
  | []   -> []
  | x::xs -> xs

let rec lookuplist symbol l = match l with
  | [] -> None
  | (a,b)::xs -> if(a = symbol) then Some(b) else lookuplist symbol xs

let rec add_entry symbol info table = match table with
| []   -> let et = empty_table
          in add_entry symbol info et
| x::xs -> (match (StringMap.find_opt symbol x) with
            |None -> (StringMap.add symbol info x)::xs
            |Some(x) -> raise DuplicateEntry)

let rec lookup (symbol : Ast.identifier) (table :'a t) = match table with
| [] -> raise NotFoundEntry
| x::xs -> ( match (StringMap.find_opt symbol x) with
              | None -> lookup symbol xs
              | Some(x) -> x 
)

let rec print_elems (table :'a t) = match table with
| [] -> Printf.fprintf stderr "Fine tabella \n"
| x::xs ->  let t1 = StringMap.bindings x in
            ignore( List.iter (fun (a,b) -> Printf.fprintf stderr "%s \n" a ) t1);
            print_elems xs

