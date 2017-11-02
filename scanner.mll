(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { commentB lexbuf }			(* Comments *)
| "//"     { commentS lexbuf }
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
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "default"{ DEFAULT }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "string" { STRING }
| "Matrix" { MATRIX }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "new"    { NEW }
| "struct" { STRUCT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['0'-'9']* '.' ['0'-'9']+ as lxm { FLOAT_LITERAL(float_of_string lxm) }
| ['a-z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and commentB = parse
  "*/" { token lexbuf }
| _    { commentB lexbuf }

and commentS = parse
  ['\n' '\r'] { token lexbuf} /* In Mac OS \r also means new line*/
| _    { commentS lexbuf }
