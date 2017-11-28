open Ast
open Sast
open Exceptions
open Utils

module StringMap = Map.Make(String)

let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(Float) || type2 = Datatype(Float)) then raise(Exceptions.InvalidBinopExpression "Cannot use equality for floats")
	else
		if type1 = type2 && (type1 = Datatype(String) || type1 = Datatype(Int)) then SBinop(se1, op, se2, type1)
		else raise (Exceptions.InvalidBinopExpression("Can only use equality operators with ints and Strings"))

let get_logical_binop_type se1 se2 op = function
		| (Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
		| (Datatype(Int), Datatype(Int))   -> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Int), Datatype(Bool))  -> SBinop(se1, op, se2, Datatype(Bool))
		| (Datatype(Bool), Datatype(Int))  -> SBinop(se1, op, se2, Datatype(Bool))
		| _ -> raise (Exceptions.InvalidBinopExpression "Can only use Bools/Ints for logical operators")

let get_arithmetic_binop_type se1 se2 op = function
		  (Datatype(Int), Datatype(Float))		-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Float), Datatype(Int))		-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(String), Datatype(String)) 	->
			(match op with
				Add -> SBinop(se1, op, se2, Datatype(String))
				| _ -> raise(Exceptions.UnsupportedStringBinop("Cannot subtract, multiply, or divide strings")))
		| (Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Matrix(typ1, i1, j1)), Datatype(Matrix(typ2, i2, j2))) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && i1=i2 && j1=j2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Exceptions.MismatchedMatricesForAddSub("Matrices must be same type and dimensions for +/-"))
				| Mult 		->
					if typ1=typ2 && j1 = i2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Exceptions.MismatchedMatricesForMult("Matrices M1(i1,j1) and M2(i2,j2) must have j1 = i2 and be of same type to be multiplied"))
				| _ -> raise(Exceptions.UnsupportedMatrixBinop("Cannot divide matrices")))
		| (Datatype(Int), Datatype(Matrix(Int,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Int, i, j)))
				| _ -> raise(Exceptions.UnsupportedMatrixBinop("Cannot add, subtract, or divide ints with matrices")))
		| (Datatype(Float), Datatype(Matrix(Float,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Float, i, j)))
				| _ -> raise(Exceptions.UnsupportedMatrixBinop("Cannot add, subtract, or divide floats with matrices")))
		| _ -> raise (Exceptions.InvalidBinopExpression("Arithmetic operators on unsupported type"))

let rec get_ID_type s func_st =
	try StringMap.find s func_st
	with | Not_found -> raise (Exceptions.UndefinedID(s))

and check_assign fname_map func_st e1 e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let type1 = get_type_from_sexpr se1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type2 = get_type_from_sexpr se2 in
	match type1, type2 with
		Datatype(String), Datatype(Int)
		| Datatype(Int), Datatype(String) -> SAssign(se1, se2, type1)
		| Datatype(Int), Datatype(Bool) -> SAssign(se1, se2, type1)
		| Datatype(Bool), Datatype(Int) -> SAssign(se1, se2, type1)
		| _ ->
	if type1 = type2
		then SAssign(se1, se2, type1)
	else raise(Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))

and check_unop fname_map func_st op e =
	let check_num_unop t = function
		  Neg -> t
		| Inc -> t
		| Dec -> t
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let check_bool_unop x = match x with
			Not 	-> Datatype(Bool)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_type_from_sexpr se in
		match t with
		  Datatype(Int)
		| Datatype(Float) -> SUnop(op, se, check_num_unop t op)
		| Datatype(Bool)  -> SUnop(op, se, check_bool_unop op)
		| _ 			  -> raise(Exceptions.InvalidUnaryOperation)

and check_binop fname_map func_st e1 op e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq when type1 = type2 && (type1 = Datatype(Int) || type1 = Datatype(Float)) -> SBinop(se1, op, se2, type1)
	| Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))

and check_expr_is_int func_st e = match e with
	NumLit(IntLit(n)) -> Datatype(Int)
	| Id(s) 			-> get_ID_type s func_st
	| _ -> raise(Exceptions.MatrixDimensionMustBeInt)

and check_matrix_access fname_map func_st name dim1 dim2 =
	ignore(check_expr_is_int func_st dim1);
	ignore(check_expr_is_int func_st dim2);
	let t = get_ID_type name func_st	in
		match t with
			Datatype(Matrix(d,rows,cols)) ->
				SMatrixAccess(name, expr_to_sexpr fname_map func_st dim1, expr_to_sexpr fname_map func_st dim2, Datatype(d))
			| _ -> raise(Exceptions.MatrixAccessOnNonMatrix(name))

and lit_to_slit n = match n with
	IntLit(n) -> SNumLit(SIntLit(n))
	| FloatLit(n) -> SNumLit(SFloatLit(n))

and typ_of_lit n = match n with
	IntLit(n) -> Datatype(Int)
	| FloatLit(n) -> Datatype(Float)

and check_matrix_lit fname_map func_st mlist =
	let smlist = (List.map (fun l -> (List.map lit_to_slit l)) mlist) in
	let first = List.hd (List.hd mlist) in
	let first_size = List.length (List.hd mlist) in
		ignore(List.iter (fun nl -> if (List.length nl = first_size) then () else raise(Exceptions.MalformedMatrixLit)) mlist);
	let first_typ = typ_of_lit first in
		ignore(List.iter (fun nl -> List.iter (fun n ->
			(let typ = typ_of_lit n in
				if (typ = first_typ)
					then ()
					else raise(Exceptions.MatrixLitMustBeOneType))) nl) mlist);
	SMatrixLit(smlist, first_typ)

and function_decl s fname_map =
	try StringMap.find s fname_map
	with Not_found -> raise (Exceptions.FunctionNotFound(s))

and check_rows s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(_, r, _)) -> (match r with IntLit(n) -> SRows(n) | _ -> raise(Exceptions.MatrixDimensionMustBeInt))
			| _ -> raise(Exceptions.CannotUseRowsOnNonMatrix(s))

and check_cols s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(_, _, c)) -> (match c with IntLit(n) -> SCols(n) | _ -> raise(Exceptions.MatrixDimensionMustBeInt))
			| _ -> raise(Exceptions.CannotUseColsOnNonMatrix(s))

and check_transpose s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(d, r, c)) -> STranspose(s, Datatype(Matrix(d,c,r)))
			| _ -> raise(Exceptions.CannotUseTransposeOnNonMatrix(s))

and expr_to_sexpr fname_map func_st = function
	  NumLit(IntLit(n))  		-> SNumLit(SIntLit(n))
	| NumLit(FloatLit(n))		-> SNumLit(SFloatLit(n))
	| BoolLit(b)       		-> SBoolLit(b)
	| StringLit(s)        		-> SStringLit(s)
	| Id(s)                		-> SId(s, get_ID_type s func_st)
	| Null                 		-> SNull
	| Noexpr               		-> SNoexpr
	| Unop(op, e)          		-> check_unop fname_map func_st op e
	| Assign(s, e)   			-> check_assign fname_map func_st s e
	| Binop(e1, op, e2)    		-> check_binop fname_map func_st e1 op e2
	| Call(s, el)				-> let fd = function_decl s fname_map in
		if List.length el != List.length fd.formals then
			raise (Exceptions.IncorrectNumberOfArguments(fd.fname, List.length el, List.length fd.formals))
		else
		SCall(s, List.map (expr_to_sexpr fname_map func_st) el, fd.return_type)
	| MatrixAccess(name, dim1, dim2)	-> check_matrix_access fname_map func_st name dim1 dim2
	| MatrixLit(nll)			-> check_matrix_lit fname_map func_st nll
	| Rows(s)					-> check_rows s func_st
	| Cols(s)					-> check_cols s func_st
	| Transpose(s)				-> check_transpose s func_st

and get_type_from_sexpr sexpr = match sexpr with
	  SNumLit(SIntLit(_))				-> Datatype(Int)
	| SNumLit(SFloatLit(_))				-> Datatype(Float)
	| SBoolLit(_)						-> Datatype(Bool)
	| SStringLit(_) 					-> Datatype(String)
	| SNoexpr 							-> Datatype(Void)
	| SNull								-> Datatype(Void)
	| SRows(r) 							-> Datatype(Int)
	| SCols(c) 							-> Datatype(Int)
	| STranspose(_,d) 					-> d
	| SId(_, d) 						-> d
	| SBinop(_, _, _, d) 				-> d
	| SAssign(_, _, d) 					-> d
	| SCall(_, _, d)					-> d
	| SUnop(_, _, d) 					-> d
	| SMatrixAccess(_, _, _, d)			-> d
	| SMatrixLit(sll, d)				->
		let c = List.length (List.hd sll) in
		let r = List.length sll in
		(match d with
			Datatype(Int) 		-> Datatype(Matrix(Int, IntLit(r), IntLit(c)))
			| Datatype(Float)	-> Datatype(Matrix(Float, IntLit(r), IntLit(c)))
			| _ 				-> raise(Exceptions.UnsupportedMatrixType))

let add_reserved_functions =
	let reserved_stub name return_type formals =
		{
			return_type	= return_type;
			fname 		= name;
			formals 	= formals;
			locals		= [];
			body 		= [];
		}
	in
	let void_t = Datatype(Void) in
	let str_t = Datatype(String) in
	let i32_t = Datatype(Int) in
	let float_t = Datatype(Float) in
	let mf t n = Formal(t, n) in (* Make formal *)
	let reserved = [
		reserved_stub "printStr" 	(void_t) 	([mf str_t "string_in"]);
		reserved_stub "printInt"		(void_t)	([mf i32_t "int_in"]);
		reserved_stub "printFloat"		(void_t)	([mf float_t "float_in"]);
	] in
	reserved

(* Variable Declaration Checking Functions *)
let report_duplicate s li =
	let rec helper = function
		n1 :: n2 :: _ when n1 = n2 ->
			if s = "global" then raise(Exceptions.DuplicateGlobal(n1))
			else if s = "function" then raise(Exceptions.DuplicateFuncOrLocal(n1))
		| _ :: t -> helper t
		| [] -> ()
	in helper (List.sort compare li)

let check_not_void s var_decl =
	match var_decl with
		(Datatype(Void), n) 	->
			if s = "global" then raise(Exceptions.VoidGlobal(n))
			else if s = "function" then raise(Exceptions.VoidFunc(n))
		| _ 					-> ()

let check_not_void_formal form =
	match form with
		Formal(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionFormal(n))
		| _ 					 	-> ()

let check_not_void_local local =
	match local with
		Local(Datatype(Void), n) 	-> raise(Exceptions.VoidFunctionLocal(n))
		| _ 						-> ()

let add_to_global_symbol_table globs =
	List.fold_left
		(fun m (t,n) -> StringMap.add n t m) StringMap.empty globs

let get_global_id = function
	| (Datatype(p), n) -> n

let get_formal_id = function
	| Formal(Datatype(p), n) -> n

let get_formal_type = function
	| Formal(Datatype(p), n) -> Datatype(p)

let get_local_id = function
	| Local(Datatype(p), n) -> n

let get_local_type = function
	| Local(Datatype(p), n) -> Datatype(p)

let check_var_decls globals =
	ignore(List.iter (check_not_void "global") globals);
	ignore(report_duplicate "global" (List.map snd globals));
	add_to_global_symbol_table globals;;

(* Function Declaration Checking Functions *)
let fdecl_to_func_st globals fdecl =
	let ffunc_st = List.fold_left (fun m f -> StringMap.add (get_formal_id f) (get_formal_type f) m) StringMap.empty fdecl.formals in
		let lffunc_st = List.fold_left (fun m l -> StringMap.add (get_local_id l) (get_local_type l) m) ffunc_st fdecl.locals in
			List.fold_left (fun m g -> StringMap.add (snd g) (fst g) m) lffunc_st globals

let rec stmt_to_sstmt fname_map func_st = function
	  Return(e)				-> SReturn(expr_to_sexpr fname_map func_st e)
	| Block(sl) 			-> SBlock(convert_stmt_list_to_sstmt_list fname_map func_st sl)
	| Expr(e) 				-> SExpr(expr_to_sexpr fname_map func_st e)
	| If(e, s1, s2) 		-> SIf((expr_to_sexpr fname_map func_st e), (stmt_to_sstmt fname_map func_st s1), (stmt_to_sstmt fname_map func_st s2))
	| For(e1, e2, e3, s) 	-> SFor((expr_to_sexpr fname_map func_st e1), (expr_to_sexpr fname_map func_st e2), (expr_to_sexpr fname_map func_st e3), (stmt_to_sstmt fname_map func_st s))
	| While(e, s)			-> SWhile((expr_to_sexpr fname_map func_st e), (stmt_to_sstmt fname_map func_st s))

and convert_stmt_list_to_sstmt_list fname_map func_st stmt_list = List.map (stmt_to_sstmt fname_map func_st) stmt_list

let fdecls_to_fname_map fdecls =
	List.fold_left
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty (fdecls @ add_reserved_functions)

let convert_fdecl_to_sfdecl globs fname_map fdecl =
	{
		sfname 			= fdecl.fname;
		sreturn_type 	= fdecl.return_type;
		sformals 		= fdecl.formals;
		slocals 		= fdecl.locals;
		sbody 			= (convert_stmt_list_to_sstmt_list fname_map (fdecl_to_func_st globs fdecl) fdecl.body);
	}

let check_function_return fname fbody returnType =
	let len = List.length fbody in
		if len > 0
			then let final_stmt = List.hd (List.rev fbody) in
				match returnType, final_stmt with
					Datatype(Void), Return(_) -> raise(Exceptions.AllVoidFunctionsMustNotReturn(fname))
					| Datatype(Void), _ 	  -> ()
					| _, Return(_)	 	      -> ()
					| _, _					  -> raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))
			else
				if returnType = Datatype(Void) then ()
				else raise(Exceptions.AllNonVoidFunctionsMustEndWithReturn(fname))

let check_return fname_map fdecl func_st e =
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_type_from_sexpr se in
		(match fdecl.return_type with
			Datatype(Matrix(d,IntLit(0),IntLit(0))) ->
			 	(match t with
					Datatype(Matrix(d,_,_)) -> ()
					| _ -> raise(Exceptions.ReturnTypeMismatch(Utils.string_of_datatype t, Utils.string_of_datatype fdecl.return_type)))
			| _ -> if (t=fdecl.return_type) then () else raise(Exceptions.ReturnTypeMismatch(Utils.string_of_datatype t, Utils.string_of_datatype fdecl.return_type)))

let rec check_stmt globs fname_map fdecl = function
	Return(e) 					-> check_return fname_map fdecl (fdecl_to_func_st globs fdecl) e
	| Block(sl) 				-> check_fbody globs fname_map fdecl sl
	| If(e, s1, s2) 			-> check_if globs fname_map fdecl s1 s2
	| While(e, s)				-> check_while globs fname_map fdecl s
	| For(e1, e2, e3, s)		-> check_for globs fname_map fdecl s
	| Expr(e)					-> ()

and check_fbody globs fname_map fdecl fbody =
	ignore(List.iter (check_stmt globs fname_map fdecl) fbody);

and check_if globs fname_map fdecl s1 s2 =
	ignore(check_stmt globs fname_map fdecl s1);
	ignore(check_stmt globs fname_map fdecl s2);

and check_while globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);

and check_for globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);

and check_else globs fname_map fdecl stmt =
	ignore(check_stmt globs fname_map fdecl stmt);;

let check_function globals fname_map global_st fdecl =
	ignore(List.iter check_not_void_formal fdecl.formals);
	ignore(List.iter check_not_void_local fdecl.locals);
	ignore(report_duplicate "function" ((List.map get_formal_id fdecl.formals) @ (List.map get_local_id fdecl.locals) @ (List.map get_global_id globals)));
	ignore(check_function_return fdecl.fname fdecl.body fdecl.return_type);
	ignore(check_fbody globals fname_map fdecl fdecl.body);;

let check_functions global_st globals fdecls =
	let sast =
		let fname_map = fdecls_to_fname_map fdecls in
		ignore(report_duplicate "function" (List.map (fun fd -> fd.fname) fdecls));
		ignore(List.iter (check_function globals fname_map global_st) fdecls);
		let sfdecls = List.map (convert_fdecl_to_sfdecl globals fname_map) fdecls in
		(globals, sfdecls)
	in sast
