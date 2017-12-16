(* Binary Operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

(* Unary Operators *)
type uop = Neg | Not | Inc | Dec

(* Nums *)
type num = IntLit of int | FloatLit of float

(* Data Types *)
type primitives = Int | Float | Bool | Void	| String | Matrix of primitives * num * num
type datatype = Datatype of primitives

(* Bind *)
type bind = datatype * string

(* Expressions *)
type expr =
	| NumLit of num
	| BoolLit of bool
	| StringLit of string
	| MatrixLit of num list list
	| Id of string
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of expr * expr
	| Call of string * expr list
	| Noexpr
	| MatrixAccess of string * expr * expr
	| Rows of string
	| Cols of string
	| Transpose of string
	| SubMatrix of string * expr * expr * expr * expr
	| Trace of string

(* Statements *)
type stmt =
	| Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt

(* Function Declarations *)
type func_decl = {
	typ 		: datatype;
	fname 		: string;
	formals 	: bind list;
	locals  	: bind list;
	body 		: stmt list;
}

type program = bind list * func_decl list


(* Pretty-printing functions *)
let string_of_num = function
	IntLit(x) 	  -> string_of_int x
	| FloatLit(x) -> string_of_float x

let string_of_primitive = function
	Int				-> "int"
	| Float			-> "float"
	| Void			-> "void"
	| Bool			-> "bool"
	| String		-> "String"
	| Matrix(p,i,j) -> "matrix(" ^ (string_of_num i) ^ "," ^ (string_of_num j) ^ ")"

let string_of_datatype = function
    	Datatype(p)		-> (string_of_primitive p)

let string_of_op = function
		Add			-> "+"
	 | 	Sub			-> "-"
	 | 	Mult		-> "*"
	 | 	Div			-> "/"
	 | 	Equal		-> "=="
	 | 	Neq			-> "!="
	 | 	Less		-> "<"
	 | 	Leq			-> "<="
	 | 	Greater		-> ">"
	 | 	Geq			-> ">="
	 | 	And			-> "and"
	 | 	Or			-> "or"

let string_of_uop = function
	Not				-> "not"
	| Inc			-> "++"
	| Dec			-> "--"
	| Neg			-> "-"

let rec string_of_expr = function
	NumLit(i)					-> string_of_num i
	| BoolLit(b)				-> if b then "true" else "false"
	| StringLit(s)				-> "\"" ^ (String.escaped s) ^ "\""
	| Id(s)						-> s
	| Binop(e1, o, e2)			-> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
	| Unop(uop, e)				-> (string_of_uop uop) ^ "(" ^ string_of_expr e ^ ")"
	| Assign(e1, e2)			-> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
	| Call(f, el)				-> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Noexpr					-> ""
	| MatrixLit(el) 			-> "MatrixLit"
	| MatrixAccess (s, i, j) 	-> (s) ^ "[" ^ (string_of_expr i) ^ "," ^ (string_of_expr j) ^ "]"
	| Rows(s)					-> (s) ^ ":rows"
	| Cols(s)					-> (s) ^ ":cols"
	| Transpose(s)				-> (s) ^ ":transpose"
	| Trace(s)                  -> (s) ^ ":trace"
	| SubMatrix(s,e1,e2,e3,e4)  -> (s) ^ ":submatrix"

let rec string_of_stmt = function
	Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_datatype t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
	string_of_datatype fdecl.typ ^ " " ^
  	fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  	")\n{\n" ^
  	String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  	String.concat "" (List.map string_of_stmt fdecl.body) ^
  	"}\n"

let string_of_program (vars, funcs) = 
	String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl funcs)
