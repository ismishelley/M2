open Ast

type snum =
		SIntLit of int
	|	SFloatLit of float

type sexpr =
	  SNumLit of snum
	| SBoolLit of bool
	| SStringLit of string
	| SMatrixLit of sexpr list list * datatype
	| SId of string * datatype
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SCall of string * sexpr list * datatype
	| SNoexpr
	| SMatrixAccess of string * sexpr * sexpr * datatype
	| SRows of int
	| SCols of int
	| STranspose of string * datatype
	| SSubMatrix of string * sexpr * sexpr * sexpr * sexpr * datatype
	| STrace of string * datatype

let get_sexpr_type sexpr = match sexpr with
	SNumLit(SIntLit(_))					-> Datatype(Int)
	| SNumLit(SFloatLit(_))				-> Datatype(Float)
	| SBoolLit(_)						-> Datatype(Bool)
	| SStringLit(_) 					-> Datatype(String)
	| SNoexpr 							-> Datatype(Void)
	| SRows(r) 							-> Datatype(Int)
	| SCols(c) 							-> Datatype(Int)
	| STranspose(_,t) 					-> t
	| SId(_, t) 						-> t
	| SBinop(_, _, _, t) 				-> t
	| SAssign(_, _, t) 					-> t
	| SCall(_, _, t)					-> t
	| SUnop(_, _, t) 					-> t
	| SMatrixAccess(_, _, _, t)			-> t
	| SMatrixLit(smlist, t)				->
		let c = List.length (List.hd smlist) in
		let r = List.length smlist in
		(match t with
			Datatype(Int) 		-> Datatype(Matrix(Int, IntLit(r), IntLit(c)))
			| Datatype(Float)	-> Datatype(Matrix(Float, IntLit(r), IntLit(c)))
			| _ 				-> raise(Failure"UnsupportedMatrixType"))
	| SSubMatrix (_,_,_,_,_,t)  		-> t 
	| STrace(_,t) 						-> t

type sstmt =
	  SBlock of sstmt list
	| SExpr of sexpr
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr

type sfunc_decl = {
	styp 			: datatype;
	sfname 			: string;
	sformals 		: bind list;
	slocals  		: bind list;
	sbody 			: sstmt list;
}

type sprogram = bind list * sfunc_decl list
