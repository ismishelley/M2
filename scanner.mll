{ open Parser 
  let unescape s =
    	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
 }

let escape = '\\' ['"']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let esc = '\\' ['\\' ''' '"' 'n' 'r' 't']
let str = '"' ( (ascii | esc)* as lxm ) '"'

rule token = parse
(* Whitespace *)
[' ' '\t' '\r' '\n'] { token lexbuf } 
  
(* Comments *)
| "/*"     { blockComment lexbuf }			
| "//"     { singleComment lexbuf }

(* Operators and Separators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
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
| "bool"   { BOOL }
| "void"   { VOID }
| "string" { STRING }
| "matrix" { MATRIX }

(* Boolean Values *)
| "true"   { TRUE }
| "false"  { FALSE }

(* Literals, Identifier, EOF *)
| ['0'-'9']+ as lxm { INT_LITERAL (int_of_string lxm) }
| ['0'-'9']+ '.' ['0'-'9']+ as lxm { FLOAT_LITERAL (float_of_string lxm) }
| str { STRING_LITERAL (unescape lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }

and singleComment = parse
  ['\n' '\r'] { token lexbuf}
| _    { singleComment lexbuf }
