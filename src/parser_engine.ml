let parse lexbuf =
  try Parser.program Scanner.token lexbuf with
  | Util.Syntax_error msg -> Util.raise_syntax_error lexbuf msg
  | Parser.Error -> Util.raise_syntax_error lexbuf "Parser error"