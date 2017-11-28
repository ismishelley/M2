(* Binary Operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

(* Unary Operators *)
type uop = Neg | Not | Inc | Dec

(* Nums *)
type num = IntLit of int | FloatLit of float

(* Data Types *)
type primitives = Int | Float | Bool | Void	| String | Matrix of primitives * num * num
type datatype = Datatype of primitives

type formal = Formal of datatype * string
type local = Local of datatype * string
type var_dec = datatype * string

(* Expressions *)
type expr =
	| NumLit of num
	| BoolLit of bool
	| StringLit of string
	| MatrixLit of num list list
	| Id of string
	| Noexpr
	| Null
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of expr * expr
	| Call of string * expr list
	| MatrixAccess of string * expr * expr
	| Rows of string
	| Cols of string
	| Transpose of string

(* Statements *)
type stmt =
	| Block of stmt list
	| Expr of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| Return of expr

(* Function Declarations *)
type func_decl = {
	return_type : datatype;
	fname 		: string;
	formals 	: formal list;
	locals  	: local list;
	body 		: stmt list;
}

(* Start Symbol *)
type program = var_dec list * func_decl list
