(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
(* Whitespace *)
[' ' '\t' '\r' '\n'] { token lexbuf } 
  
(* Comments *)
| "/*"     { blockComment lexbuf }			
| "//"     { singleComment lexbuf }

(* Operators and Separators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* Control Flow *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }
| "continue" { CONTINUE }

(* Data Types *)
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "bool"   { BOOL }
| "true"   { TRUE }
| "false"  { FALSE }
| "void"   { VOID }
| "string" { STRING }
(* | "Matrix" { MATRIX } *)

(* Other Reserved Words *)
(*| "new"    { NEW }
| "struct" { STRUCT } *)

(* Literals *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['0'-'9']+ '.' ['0'-'9']+ as lxm { FLOAT_LITERAL(float_of_string lxm) }
| '"' ([^'"']|"\\\"")* '"' as lxm { STRING_LITERAL(lxm) }
| ['a-z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }

and singleComment = parse
  ['\n' '\r'] { token lexbuf}
| _    { singleComment lexbuf }
