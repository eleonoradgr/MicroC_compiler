exception DuplicateEntry
exception NotFoundEntry

type 'a t = (Ast.identifier * 'a) list list (* TODO: this is a dummy definition *)

let empty_table = [[]]

let begin_block table = []::table

let end_block table = match table with
  | []   -> []
  | x::xs -> xs

let add_entry symbol info table = match table with
| []   -> [[(symbol,info)]]
| x::xs -> ((symbol,info)::x)::xs

let rec lookuplist symbol l = match l with
  | [] -> None
  | (a,b)::xs -> if(a = symbol) then Some(b) else lookuplist symbol xs

let rec lookup (symbol : Ast.identifier) (table :'a t) = match table with
| [] -> raise NotFoundEntry
| x::xs -> ( match (lookuplist symbol x) with
              | None -> lookup symbol xs
              | Some(x) -> x 
)