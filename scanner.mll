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
| "switch" { SWITCH} /* are we doing switch statements? */
| "default"{ DEFAULT }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "int"    { INT }
| "float"  { FLOAT }
| "double" { DOUBLE }
| "char"   { CHAR }
| "string" { STRING }
| "Matrix" { MATRIX }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "new"    { NEW }
| "main"   { MAIN }
| "struct" { STRUCT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { DATAID(lxm) }
| ['a'-'z'] { MATHID }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and commentB = parse
  "*/" { token lexbuf }
| _    { commentB lexbuf }

and commentS = parse
  "\n" { token lexbuf}
| _    { commentS lexbuf }
