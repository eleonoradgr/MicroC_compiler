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
            ("null", NULL);
            ("void", VOID);
            ("int", TYPE_INT);
            ("char", TYPE_CHAR);
            ("bool", TYPE_BOOL);
            ("return", RETURN);
            ("while", WHILE);
            ("for", FOR);
            ("if", IF);
            ("else", ELSE);
  ]
}

let digit = ['0'-'9']
let character = ['_''a'-'z' 'A'-'Z']
let id = ['_''a'-'z' 'A'-'Z']['_''a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    | [' ' '\t']            { token lexbuf }
    | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
    | digit+ as inum        { let num = int_of_string inum in INT num}
    | '''character ''' as s { let c = String.get s 1 in CHAR c}
    | id as word            { try
                                let token = Hashtbl.find keyword_table word in
                                printf "keyword: %s\n" word;
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
    | "!="                  { NE }
    | "||"                  { OR }
    | "&&"                  { AND }
    | '!'                   { NOT }
    | '='                   { ASS }
    | '&'                   { ADDR }
    | '{'                   { LBRACE }
    | '}'                   { RBRACE }
    | '['                   { LBRACK }
    | ']'                   { RBRACK }
    | '('                   { LPAR }
    | ')'                   { RPAR }
    | ':'                   { COLON }
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
