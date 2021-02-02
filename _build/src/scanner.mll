{
    open Parser
    open Printf

    (* Put here your auxiliary definitions *)
    let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

    let keyword_table =
        create_hashtable 16 [
            ("true", TRUE);
            ("false", FALSE);
            ("NULL", NULL);
            ("void", VOID);
            ("int", TYPE_INT);
            ("float", TYPE_FLOAT);
            ("char", TYPE_CHAR);
            ("bool", TYPE_BOOL);
            ("return", RETURN);
            ("while", WHILE);
            ("for", FOR);
            ("if", IF);
            ("else", ELSE);
            ("do", DO);
  ]
}

let digit = ['0'-'9']
let character = ['_''a'-'z' 'A'-'Z']
let id = ['_''a'-'z' 'A'-'Z']['_''a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    | [' ' '\t']            { token lexbuf }
    | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
    | digit+ as i           { let num = int_of_string i in INT num}
    | digit+'.'digit* as f  { let num = float_of_string f in FLOAT num}
    | '''character ''' as s { let c = String.get s 1 in CHAR c}
    | '"'                   { STRING( string (Buffer.create 509 )lexbuf) }
    | id as word            { try
                                let token = Hashtbl.find keyword_table word in
                                token
                                with Not_found ->
                                    ID word}
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '%'                   { MOD }
    | "=="                  { EQ }
    | '<'                   { LESS }
    | '>'                   { GREATER }
    | "<="                  { LEQ }
    | ">="                  { GEQ }
    | "!="                  { NEQ }
    | "||"                  { OR }
    | "&&"                  { AND }
    | '!'                   { NOT }
    | '='                   { ASS }
    | "+="                  { PLUSASS }
    | "-="                  { MINUSASS }
    | "*="                  { MINUSASS }
    | "/="                  { DIVASS }
    | "%="                  { MODASS }
    | "++"                  { INC }
    | "--"                  { DEC }
    | '&'                   { ADDR }
    | '{'                   { LBRACE }
    | '}'                   { RBRACE }
    | '['                   { LBRACK }
    | ']'                   { RBRACK }
    | '('                   { LPAR }
    | ')'                   { RPAR }
    | ';'                   { SEMICOLON }
    | ','                   { COMMA }
    | eof                   { EOF }
    | "//"                  {line_comment lexbuf}
    | "/*"                  { comment lexbuf}
    | _ as c                { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and line_comment = parse
    | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
    |_                      {line_comment lexbuf}
and comment = parse
    | "*/"                  { token lexbuf }
    | _                     { comment lexbuf }
    | eof                   { Util.raise_lexer_error lexbuf (" Comment not closed") }
and string buf = parse
    | [^'"' '\n' '\\']+ as s    { Buffer.add_string buf s;
                                    string buf lexbuf }
    | '\n'                      { Buffer.add_char buf '\n';
                                    Lexing.new_line lexbuf;
                                    string buf lexbuf }
    | '\\' '"'                  { Buffer.add_char buf '"'; 
                                    string buf lexbuf }
    | '\\'                      { Buffer.add_char buf '\\';
                                    string buf lexbuf}
    | '"'                       { Buffer.contents buf } 
    | eof                       { Util.raise_lexer_error lexbuf ("end of file inside of a string") }
    | _ as c                    { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c)}    