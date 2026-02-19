{
  open Lexing
  open Parser (* Get the token types from Menhir *)
  open Option
  (* open Context  *)
  exception Error of string
  let string_buff = Buffer.create 1024

    let init _filename channel : Lexing.lexbuf = 
        Lexing.from_channel channel
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let characters = ['_' 'a'-'z' 'A'-'Z']
let identifier = characters (characters|digit)*

(* Integer constants *)
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*

(* Floating constants *)
let sign = ['-' '+']
let digit_sequence = digit+

(* Character and string constants *)
let simple_escape_sequence =
  '\\' ['\''  '\"'  '?'  '\\'  'a'  'b'  'f'  'n'  'r'  't'  'v']

let escape_sequence =
    simple_escape_sequence

rule read = parse
  | white { read lexbuf } (* Skip whitespace *)
  | '\n'  { new_line lexbuf; read lexbuf }
  | "/*"  { multiline_comment lexbuf; read lexbuf }
  | "//"  { singleline_comment lexbuf; read lexbuf }
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { MUL }
  | '/'   { DIV }
  | '%'   { MOD }
  | "+="  { ADD_ASSIGN }
  | "-="  { SUB_ASSIGN }
  | "*="  { MUL_ASSIGN }
  | "/="  { DIV_ASSIGN }
  | "%="  { MOD_ASSIGN }
  | "|="  { OR_ASSIGN  }
  | "&="  { AND_ASSIGN }
  | "^="  { XOR_ASSIGN }
  | "<<=" { SHL_ASSIGN }
  | ">>=" { SHR_ASSIGN }
  | "<<"  { SHL }
  | ">>"  { SHR }
  | '='   { EQUAL }
  | "=="  { EQUAL_EQUAL }
  | "!="  { NOT_EQUAL }
  | "<="  { LESS_OR_EQUAL }
  | ">="  { GREATER_OR_EQUAL }
  | "="   { EQUAL }
  | "<"   { LESS_THAN }
  | ">"   { GREATER_THAN }
  | "++"  { INCREMENT }
  | "--"  { DECREMENT }
  | "!"   { BANG }
  | "&&"  { AMPERSAND2 }
  | "||"  { PIPE_PIPE }
  | "&"   { AMPERSAND }
  | "|"   { PIPE }
  | "^"   { CARET }
  | "?"   { QUESTION_MARK }
  | ":"   { COLON }
  | "~"   { TILDE }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | ';'   { SEMICOLON }
  | ','   { COMMA }
  | '.'   { DOT }
  | ".."  { ELLIPSIS2 }
  | "..." { ELLIPSIS3 }
  | "=>" { ARROW }
  | "let" { LET }
  | "impl" { IMPL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "bool" { BOOL }
  | "const"   { CONST }
  | "auto"    { AUTO }
  | "break"   { BREAK }
  | "char"    { CHAR }
  | "continue"{ CONTINUE }
  | "do"      { DO }
  | "double"  { DOUBLE }
  | "else"    { ELSE }
  | "extern"  { EXTERN }
  | "float"   { FLOAT }
  | "for"     { FOR }
  | "goto"    { GOTO }
  | "if"      { IF }
  | "inline"  { INLINE }
  | "int"     { INT }
  | "long"    { LONG }
  | "return"  { RETURN }
  | "short"   { SHORT }
  | "signed"  { SIGNED }
  | "sizeof"  { SIZEOF }
  | "static"  { STATIC }
  | "enum"    { ENUM }
  | "struct"  { STRUCT }
  | "union"   { UNION }
  | "switch"  { SWITCH }
  | "typedef" { TYPEDEF }
  | "unsigned"{ UNSIGNED }
  | "void"    { VOID }
  | "defer"  { DEFER }
  | "as"     { AS }
  | "macro"  { MACRO }
  | "#include" { INCLUDE }
  (* | "volatile"                    { VOLATILE } *)
  | "while"                       { WHILE }
  | digit+ as i { NUMBER_LITERAL (int_of_string i) }
  | identifier as id   { IDENTIFIER id }
  |  "'"  { char lexbuf; char_literal_end lexbuf  ; CHAR_LITERAL (lexeme lexbuf) }
  | '"'   { Buffer.clear string_buff; read_string lexbuf}
  | eof   { EOF }
  | _     { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }


and char = parse
  | simple_escape_sequence        { }
  | '\\' _                        { failwith "incorrect escape sequence" }
  | _                             { }

and char_literal_end = parse
  | '\''       { }
  | '\n' | eof { failwith "missing terminating \"'\" character" }
  | ""         { char lexbuf; char_literal_end lexbuf }

and string_literal = parse
  | '\"'       { }
  | '\n' | eof { failwith "missing terminating '\"' character" }
  | _         {  string_literal lexbuf }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

(* This rule is only entered when we see a double quote *)
and read_string = parse
  (* Case 1: Closing Quote -> Return the accumulated string *)
  | '"' 
      { 
        let str = Buffer.contents string_buff in
        STRING_LITERAL str 
      }

  (* Case 2: Escape Sequences *)
  | '\\' '/'  { Buffer.add_char string_buff '/';  read_string lexbuf }
  | '\\' '\\' { Buffer.add_char string_buff '\\'; read_string lexbuf }
  | '\\' 'b'  { Buffer.add_char string_buff '\b'; read_string lexbuf }
  | '\\' 'f'  { Buffer.add_char string_buff '\012'; read_string lexbuf }
  | '\\' 'n'  { Buffer.add_string string_buff "\\n"; read_string lexbuf }
  | '\\' 'r'  { Buffer.add_char string_buff '\r'; read_string lexbuf }
  | '\\' 't'  { Buffer.add_char string_buff '\t'; read_string lexbuf }
  | '\\' '"'  { Buffer.add_char string_buff '"';  read_string lexbuf }
  
  (* Case 3: Regular Characters *)
  (* Match any char that ISN'T a backslash or a quote *)
  | [^ '"' '\\']+ as lxm
      { 
        Buffer.add_string string_buff lxm;
        read_string lexbuf 
      }

  (* Case 4: End of File (Error) *)
  (* If we hit EOF while inside a string, the user forgot the closing quote *)
  | eof 
      { raise (Error "String is not terminated") }
  
  (* Case 5: Catch-all for weird chars *)
  | _ as char
      { 
        Buffer.add_char string_buff char;
        read_string lexbuf 
      }
