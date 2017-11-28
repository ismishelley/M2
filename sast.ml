open Ast

type snum =
		SIntLit of int
	|	SFloatLit of float

(* Expressions *)
type sexpr =
	  SNumLit of snum
	| SBoolLit of bool
	| SStringLit of string
	| SMatrixLit of sexpr list list * datatype
	| SId of string * datatype
	| SNoexpr
	| SNull
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SCall of string * sexpr list * datatype
	| SMatrixAccess of string * sexpr * sexpr * datatype
	| SRows of int
	| SCols of int
	| STranspose of string * datatype

(* Statements *)
type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

(* Function Declarations *)
type sfunc_decl = {
	sreturn_type 	: datatype;
	sfname 			: string;
	sformals 		: formal list;
	slocals  		: local list;
	sbody 			: sstmt list;
}

(* All method declarations | Main entry method *)
type sprogram = var_dec list * func_decl list
