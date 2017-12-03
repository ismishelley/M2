open Ast
open Sast
open Utils

module StringMap = Map.Make(String)

let get_equality_binop_type type1 type2 se1 se2 op =
	if (type1 = Datatype(String) || type1 = Datatype(Int)|| type1 = Datatype(Float)) && type1 = type2 
	then SBinop(se1, op, se2, type1)
	else raise (Failure "Equality operator only operates on int, float, and String")

let get_logical_binop_type se1 se2 op = function
		| (Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
		| (Datatype(Int), Datatype(Int))   -> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Int), Datatype(Bool))  -> SBinop(se1, op, se2, Datatype(Bool))
		| (Datatype(Bool), Datatype(Int))  -> SBinop(se1, op, se2, Datatype(Bool))
		| _ -> raise (Failure "Can only use Bools/Ints for logical operators")

let get_arithmetic_binop_type se1 se2 op = function
		  (Datatype(Int), Datatype(Float))		-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Float), Datatype(Int))		-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(String), Datatype(String)) 	->
			(match op with
				Add -> SBinop(se1, op, se2, Datatype(String))
				| _ -> raise(Failure("Cannot subtract, multiply, or divide strings")))
		| (Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Matrix(typ1, i1, j1)), Datatype(Matrix(typ2, i2, j2))) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && i1=i2 && j1=j2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Failure("Matrices must be same type and dimensions for +/-"))
				| Mult 		->
					if typ1=typ2 && j1 = i2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Failure("Matrices M1(i1,j1) and M2(i2,j2) must have j1 = i2 and be of same type to be multiplied"))
				| _ -> raise(Failure("Cannot divide matrices")))
		| (Datatype(Int), Datatype(Matrix(Int,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Int, i, j)))
				| _ -> raise(Failure("Cannot add, subtract, or divide ints with matrices")))
		| (Datatype(Float), Datatype(Matrix(Float,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Float, i, j)))
				| _ -> raise(Failure("Cannot add, subtract, or divide floats with matrices")))
		| _ -> raise (Failure("Arithmetic operators on unsupported type"))

let rec get_ID_type s func_st =
	try StringMap.find s func_st
	with | Not_found -> raise (Failure("Undefined ID " ^ s))

and check_assign fname_map func_st e1 e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let type1 = get_sexpr_type se1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type2 = get_sexpr_type se2 in
	match type1, type2 with
		Datatype(String), Datatype(Int)
		| Datatype(Int), Datatype(String) -> SAssign(se1, se2, type1)
		| Datatype(Int), Datatype(Bool) -> SAssign(se1, se2, type1)
		| Datatype(Bool), Datatype(Int) -> SAssign(se1, se2, type1)
		| _ ->
	if type1 = type2
		then SAssign(se1, se2, type1)
	else raise(Failure "mismatched assignment type")

and check_unop fname_map func_st op e =
	let check_num_unop t = function
		  Neg -> t
		| Inc -> t
		| Dec -> t
		| 	_ 		-> raise(Failure"InvalidUnaryOperation")
	in
	let check_bool_unop x = match x with
			Not 	-> Datatype(Bool)
		| 	_ 		-> raise(Failure"InvalidUnaryOperation")
	in
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_sexpr_type se in
		match t with
		  Datatype(Int)
		| Datatype(Float) -> SUnop(op, se, check_num_unop t op)
		| Datatype(Bool)  -> SUnop(op, se, check_bool_unop op)
		| _ 			  -> raise(Failure"InvalidUnaryOperation")

and check_binop fname_map func_st e1 op e2 =
	let se1 = expr_to_sexpr fname_map func_st e1 in
	let se2 = expr_to_sexpr fname_map func_st e2 in
	let type1 = get_sexpr_type se1 in
	let type2 = get_sexpr_type se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq when type1 = type2 && (type1 = Datatype(Int) || type1 = Datatype(Float)) -> SBinop(se1, op, se2, type1)
	| Add | Mult | Sub | Div -> get_arithmetic_binop_type se1 se2 op (type1, type2)
	| _ -> raise (Failure ((Utils.string_of_op op) ^ " is not a supported binary op"))

and check_expr_is_int func_st e = match e with
	NumLit(IntLit(n)) -> Datatype(Int)
	| Id(s) 			-> get_ID_type s func_st
	| _ -> raise(Failure"MatrixDimensionMustBeInt")

and check_matrix_access fname_map func_st name dim1 dim2 =
	ignore(check_expr_is_int func_st dim1);
	ignore(check_expr_is_int func_st dim2);
	let t = get_ID_type name func_st	in
		match t with
			Datatype(Matrix(d,rows,cols)) ->
				SMatrixAccess(name, expr_to_sexpr fname_map func_st dim1, expr_to_sexpr fname_map func_st dim2, Datatype(d))
			| _ -> raise(Failure "MatrixAccessOnNonMatrix")

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
		ignore(List.iter (fun nl -> if (List.length nl = first_size) then () else raise(Failure"MalformedMatrixLit")) mlist);
	let first_typ = typ_of_lit first in
		ignore(List.iter (fun nl -> List.iter (fun n ->
			(let typ = typ_of_lit n in
				if (typ = first_typ)
					then ()
					else raise(Failure"MatrixLitMustBeOneType"))) nl) mlist);
	SMatrixLit(smlist, first_typ)

(* check if s is the name of a declared function *)
and function_decl s fname_map =
	try StringMap.find s fname_map
	with Not_found -> raise (Failure "FunctionNotFound")

and check_rows s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(_, r, _)) -> (match r with IntLit(n) -> SRows(n) | _ -> raise(Failure "MatrixDimensionMustBeInt"))
			| _ -> raise(Failure"CannotUseRowsOnNonMatrix")

and check_cols s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(_, _, c)) -> (match c with IntLit(n) -> SCols(n) | _ -> raise(Failure"MatrixDimensionMustBeInt"))
			| _ -> raise(Failure"CannotUseColsOnNonMatrix")

and check_transpose s func_st =
	let typ = get_ID_type s func_st in
		match typ with
			Datatype(Matrix(d, r, c)) -> STranspose(s, Datatype(Matrix(d,c,r)))
			| _ -> raise(Failure"CannotUseTransposeOnNonMatrix")

and expr_to_sexpr fname_map func_st = function
	  NumLit(IntLit(n))  		-> SNumLit(SIntLit(n))
	| NumLit(FloatLit(n))		-> SNumLit(SFloatLit(n))
	| BoolLit(b)       			-> SBoolLit(b)
	| StringLit(s)        		-> SStringLit(s)
	| Id(s)                		-> SId(s, get_ID_type s func_st)
	| Null                 		-> SNull
	| Noexpr               		-> SNoexpr
	| Unop(op, e)          		-> check_unop fname_map func_st op e
	| Assign(s, e)   			-> check_assign fname_map func_st s e
	| Binop(e1, op, e2)    		-> check_binop fname_map func_st e1 op e2
	| Call(s, el)				-> let fd = function_decl s fname_map in
		if List.length el != List.length fd.formals then
			raise (Failure "IncorrectNumberOfArguments")
		else
		SCall(s, List.map (expr_to_sexpr fname_map func_st) el, fd.return_type)
	| MatrixAccess(name, dim1, dim2)	-> check_matrix_access fname_map func_st name dim1 dim2
	| MatrixLit(nll)			-> check_matrix_lit fname_map func_st nll
	| Rows(s)					-> check_rows s func_st
	| Cols(s)					-> check_cols s func_st
	| Transpose(s)				-> check_transpose s func_st

and get_sexpr_type sexpr = match sexpr with
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
			| _ 				-> raise(Failure"UnsupportedMatrixType"))

(* add built-in print functions *)
let built_in_decls = 
	let declare_func name return_type formals =
		{
			return_type	= return_type;
			fname 		= name;
			formals 	= formals;
			locals		= [];
			body 		= [];
		} 
	in
	let print_func_decls = [
		declare_func "printStr" 	(Datatype(Void)) 	([(Datatype(String), "string_in")]);
		declare_func "printInt"		(Datatype(Void))	([(Datatype(Int), "int_in")]);
		declare_func "printFloat"	(Datatype(Void))	([(Datatype(Float), "float_in")]);
	] 
	in print_func_decls

(* Variable Declaration Checking Functions *)
(* Raise an exception if the given list has a duplicate *)
let report_duplicate exceptf list =
	let rec helper = function
		n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
		| _ :: t -> helper t
		| [] -> ()
	in helper (List.sort compare list)

(* Raise an exception if a given binding is to a void type *)
let check_not_void exceptf = function
      (Datatype(Void), n) -> raise (Failure (exceptf n))
    | _ -> ()

let check_not_void_fl = function
	(Datatype(Void), n) -> raise(Failure("Void formal/local"))
	| _ 				-> ()

let add_to_global_symbol_table globs =
	List.fold_left
		(fun m (t,n) -> StringMap.add n t m) StringMap.empty globs

let get_global_id = function
	| (Datatype(p), n) -> n

let check_var_decls globals =
	ignore(List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals);
	(* check for duplicate global variables *)
	report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
	add_to_global_symbol_table globals;;

(* Function Declaration Checking Functions *)
(* Build function symbol table *)
let fdecl_to_func_st globals fdecl =
	List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty (globals @ fdecl.formals @ fdecl.locals)

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
		(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty (fdecls @ built_in_decls)

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
		if len > 0 then let final_stmt = List.hd (List.rev fbody) in
				match returnType, final_stmt with
					Datatype(Void), Return(_) -> raise(Failure("Void functions should not return anything"))
					| Datatype(Void), _ 	  -> ()
					| _, Return(_)	 	      -> ()
					| _, _					  -> raise(Failure("Missing return statement"))
		else
			if returnType = Datatype(Void) then ()
			else raise(Failure("Missing return statement"))

let check_return fname_map fdecl func_st e =
	let se = expr_to_sexpr fname_map func_st e in
	let t = get_sexpr_type se in
		(match fdecl.return_type with
			Datatype(Matrix(d,IntLit(0),IntLit(0))) ->
			 	(match t with
					Datatype(Matrix(d,_,_)) -> ()
					| _ -> raise(Failure "Mismatch return type" ))
			| _ -> if (t=fdecl.return_type) then () else raise(Failure "Mismatch return type" ))

(* check statement *)
let rec check_stmt globs fname_map fdecl = function
	Block(sl) 				-> check_fbody globs fname_map fdecl sl
	| Expr(e)				-> ()
	| Return(e) 			-> check_return fname_map fdecl (fdecl_to_func_st globs fdecl) e
	| If(e, s1, s2) 		-> check_stmt globs fname_map fdecl s1; 
								check_stmt globs fname_map  fdecl s2;
	| For(e1, e2, e3, s)	-> check_stmt globs fname_map fdecl s
	| While(e, s)			-> check_stmt globs fname_map fdecl s

and check_fbody globs fname_map fdecl fbody =
	ignore(List.iter (check_stmt globs fname_map fdecl) fbody);;

(* several checks on a single function declaration *)
let check_function globals fname_map global_symtbl fdecl =
	(* check void for formals and locals *)
	List.iter check_not_void_fl (fdecl.formals @ fdecl.locals);	

	(* check for duplicate variables *)
	report_duplicate (fun n -> "duplicate global variable " ^ n) (List.map snd globals);
	report_duplicate (fun n -> "duplicate formal variable " ^ n) (List.map snd fdecl.formals);
	report_duplicate (fun n -> "duplicate local variable " ^ n) (List.map snd fdecl.locals);

	(* check return statement of the function *)
	ignore(check_function_return fdecl.fname fdecl.body fdecl.return_type);

	(* check body of the function *)
	ignore(check_fbody globals fname_map fdecl fdecl.body);;

(* build sast *)
let check_functions global_symtbl globals functions =
	let sast =
		(* Convert function decls to a StringMap of (name,decl) pairs*)
		let fname_map = fdecls_to_fname_map functions in
		(* check for duplicate function names *)
		report_duplicate (fun n -> "duplicate function " ^ n) (List.map (fun fd -> fd.fname) functions);
		ignore(List.iter (check_function globals fname_map global_symtbl) functions);
		let sfdecls = List.map (convert_fdecl_to_sfdecl globals fname_map) functions in
		(globals, sfdecls)
	in sast
