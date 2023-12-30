let parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  ErrorMsg.fileName := filename;
  try
    let ast = Parser.program Lexer.tokenize lexbuf in
    ast
  with Parsing.Parse_error -> ErrorMsg.error (-1) "Failed to parse file."

let parse_string str =
  let lexbuf = Lexing.from_string str in
  ErrorMsg.fileName := "<string>";
  try Parser.program Lexer.tokenize lexbuf
  with Parsing.Parse_error -> ErrorMsg.error (-1) "Failed to parse file."
