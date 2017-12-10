open Ast
open Sast

module StringMap = Map.Make(String)

let getEqualType se1 se2 op = function
		(Datatype(Int),Datatype(Int)) -> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(Float), Datatype(Float)) -> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(String), Datatype(String)) -> SBinop(se1, op, se2, Datatype(String))
		| _ -> raise (Failure "Invalid type for equality operators")

let getLogicalType se1 se2 op = function
		(Datatype(Bool), Datatype(Bool)) -> SBinop(se1, op, se2, Datatype(Bool))
		| _ -> raise (Failure "Invalid type for logical operators")

let getArithmeticType se1 se2 op = function
		  (Datatype(Int), Datatype(Float)) 
		| (Datatype(Float), Datatype(Int)) 
		| (Datatype(Float), Datatype(Float)) 	-> SBinop(se1, op, se2, Datatype(Float))
		| (Datatype(Int), Datatype(Int)) 		-> SBinop(se1, op, se2, Datatype(Int))
		| (Datatype(String), Datatype(String)) 	->
			(match op with
				Add -> SBinop(se1, op, se2, Datatype(String))
				| _ -> raise(Failure "Invalid operation on String"))
		| (Datatype(Matrix(typ1, i1, j1)), Datatype(Matrix(typ2, i2, j2))) ->
			(match op with
				Add | Sub 	->
					if typ1=typ2 && i1=i2 && j1=j2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Failure "Incorrect dimention/type for matrix addition/subtract")
				| Mult 		->
					if typ1=typ2 && j1 = i2 then
						SBinop(se1, op, se2, Datatype(Matrix(typ1, i1, j2)))
					else raise(Failure "Incorrect dimention/type for matrix multiplication")
				| _ -> raise(Failure("Invalid operation on matrix")))
		| (Datatype(Int), Datatype(Matrix(Int,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Int, i, j)))
				| _ -> raise(Failure "Invalid operation between integer and matrix"))
		| (Datatype(Float), Datatype(Matrix(Float,i,j))) ->
			(match op with
				Mult -> SBinop(se1, op, se2, Datatype(Matrix(Float, i, j)))
				| _ -> raise(Failure("Invalid operation between float and matrix")))
		| _ -> raise (Failure("Invalid type for arithmetic operators"))


(* top-level checking function *)
let check (globals, functions) =

	(* Function for checking duplicates *)
	let report_duplicate exceptf list =
		let rec helper = function
			n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
			| _ :: t -> helper t
			| [] -> ()
		in helper (List.sort compare list)
	in

	(* Function for checking void type in binding *)
	let check_not_void exceptf = function
	      (Datatype(Void), n) -> raise (Failure (exceptf n))
	    | _ -> ()

	in

	(**** Checking Global Variables ****)
	List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
	report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
	

	(**** Checking Functions ****)
	if List.mem "printInt" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function printInt may not be defined")) else ();
  	if List.mem "printFloat" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function printFloat may not be defined")) else ();
  	if List.mem "printStr" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function printStr may not be defined")) else ();
  	if List.mem "printBool" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function printBool may not be defined")) else ();
  	if List.mem "printbig" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function printbig may not be defined")) else ();

  	(* check for duplicate function names *)
	report_duplicate (fun n -> "duplicate function " ^ n) (List.map (fun fd -> fd.fname) functions);

	(* add built-in print functions *)
	let declare_func name return_type formals =
			{
				typ	= return_type;
				fname 		= name;
				formals 	= formals;
				locals		= [];
				body 		= [];
			} 
	in
	
	let built_in_decls = [
			declare_func "printStr" 	(Datatype(Void)) 	([(Datatype(String), "string_in")]);
			declare_func "printInt"		(Datatype(Void))	([(Datatype(Int), "int_in")]);
			declare_func "printFloat"	(Datatype(Void))	([(Datatype(Float), "float_in")]);
			declare_func "printBool"	(Datatype(Void))	([(Datatype(Bool), "bool_in")]);
			declare_func "printbig"	    (Datatype(Void))	([(Datatype(Int), "big_in")]);
	]
	in 	

	let function_decls = List.fold_left
			(fun m fd -> StringMap.add fd.fname fd m) StringMap.empty (functions @ built_in_decls)
	in

	(* check if s is the name of a declared function *)
	let function_decl s =
		try StringMap.find s function_decls
		with Not_found -> raise (Failure "FunctionNotFound")
	in

	let _ = function_decl "main" in

	(* several checks on a single function declaration *)
	let check_function func =
		(* check void/duplicate for formals and locals *)
		List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^ " in " ^ func.fname)) func.formals;
    	List.iter (check_not_void (fun n -> "illegal void local " ^ n ^ " in " ^ func.fname)) func.locals;
		report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname) (List.map snd func.formals);
    	report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname) (List.map snd func.locals);

		(* check for duplicate variables *)
		ignore(report_duplicate (fun n -> "duplicate global variable " ^ n) (List.map snd globals));
		ignore(report_duplicate (fun n -> "duplicate formal variable " ^ n) (List.map snd func.formals));
		ignore(report_duplicate (fun n -> "duplicate local variable " ^ n) (List.map snd func.locals));
	in


	(* Build function symbol table *)
	let func_to_symbols func =
		List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty (globals @ func.formals @ func.locals)
	in

	let rec type_of_identifier s symbols =
		try StringMap.find s symbols
		with | Not_found -> raise (Failure("Undefined ID " ^ s))

	and check_expr_is_int symbols e = match e with
		NumLit(IntLit(n)) -> Datatype(Int)
		| Id(s) 			-> type_of_identifier s symbols
		| _ -> raise(Failure"Integer required for matrix dimension")

	and lit_to_slit n = match n with
		IntLit(n) -> SNumLit(SIntLit(n))
		| FloatLit(n) -> SNumLit(SFloatLit(n))

	and typ_of_lit n = match n with
		IntLit(n) -> Datatype(Int)
		| FloatLit(n) -> Datatype(Float)

	and sexpr symbols = function
		  NumLit(IntLit(n))  		-> SNumLit(SIntLit(n))
		| NumLit(FloatLit(n))		-> SNumLit(SFloatLit(n))
		| BoolLit(b)       			-> SBoolLit(b)
		| StringLit(s)        		-> SStringLit(s)
		| Id(s)                		-> SId(s, type_of_identifier s symbols)
		| Null                 		-> SNull
		| Noexpr               		-> SNoexpr
		| Unop(op, e)          		-> let se = sexpr symbols e in
										let typ = Sast.get_sexpr_type se in
										(match op with
											Neg when typ = Datatype(Int) -> SUnop(op, se ,typ)
											| Neg when typ = Datatype(Float) -> SUnop(op, se ,typ)
											| Inc when typ = Datatype(Int) -> SUnop(op, se ,typ)
											| Inc when typ = Datatype(Float) -> SUnop(op, se ,typ)
											| Dec when typ = Datatype(Int) -> SUnop(op, se ,typ)
											| Dec when typ = Datatype(Float) -> SUnop(op, se ,typ)
											| Not when typ = Datatype(Bool) -> SUnop(op, se, typ)
											| _ -> raise(Failure "Invalid datatype for unop")
										)
		| Assign(s,e) 				-> 	let se1 = sexpr symbols s in
										let se2 = sexpr symbols e in
										let type1 = Sast.get_sexpr_type se1 in
										let type2 = Sast.get_sexpr_type se2 in
											(if type1 = type2
												then SAssign(se1, se2, type1)
											else raise(Failure "Mismatched assignment type"))
		| Binop(e1, op, e2)			-> 	let se1 = sexpr symbols e1 in
										let se2 = sexpr symbols e2 in
										let type1 = Sast.get_sexpr_type se1 in
										let type2 = Sast.get_sexpr_type se2 in
											(match op with
											Equal | Neq -> getEqualType se1 se2 op (type1, type2)
											| And | Or -> getLogicalType se1 se2 op (type1, type2)
											| Less | Leq | Greater | Geq when type1 = type2 && (type1 = Datatype(Int) || type1 = Datatype(Float)) -> SBinop(se1, op, se2, type1)
											| Add | Mult | Sub | Div -> getArithmeticType se1 se2 op (type1, type2)
											| _ -> raise (Failure "Invalid binary operator"))
		| Call(fname, actuals)		-> let fd = function_decl fname in
										if List.length actuals != List.length fd.formals then
											raise (Failure "Incorrect number of arguments")
								 		else
								 			SCall(fname, List.map (sexpr symbols) actuals, fd.typ)
		| MatrixAccess(s, dim1, dim2)	-> 	ignore(check_expr_is_int symbols dim1);
											ignore(check_expr_is_int symbols dim2);
											let typ = type_of_identifier s symbols	in
												(match typ with
													Datatype(Matrix(d,rows,cols)) ->
														SMatrixAccess(s, sexpr symbols dim1, sexpr symbols dim2, Datatype(d))
													| _ -> raise(Failure "Cannot operate on nonmatrix")	)
		| MatrixLit(mlist)			-> let smlist = (List.map (fun l -> (List.map lit_to_slit l)) mlist) in
										let first = List.hd (List.hd mlist) in
										let first_size = List.length (List.hd mlist) in
											ignore(List.iter (fun nl -> if (List.length nl = first_size) then () else raise(Failure "Rows of matrix must have the same size")) mlist);
										let first_typ = typ_of_lit first in
											ignore(List.iter (fun nl -> List.iter (fun n ->
												(let typ = typ_of_lit n in
													if (typ = first_typ)
														then ()
														else raise(Failure "More than one datatype in a matrix" ))) nl) mlist);
										SMatrixLit(smlist, first_typ)
		| Rows(s)					-> let typ = type_of_identifier s symbols in
											(match typ with
												Datatype(Matrix(_, r, _)) -> 
													(match r with IntLit(n) -> SRows(n) 
														| _ -> raise(Failure "Integer required for matrix dimension"))
												| _ -> raise(Failure"Cannot operate on nonmatrix"))
		| Cols(s)					-> let typ = type_of_identifier s symbols in
										(match typ with
											Datatype(Matrix(_, _, c)) -> 
												(match c with IntLit(n) -> SCols(n) 
															| _ -> raise(Failure"Integer required for matrix dimension"))
											| _ -> raise(Failure"Cannot operate on nonmatrix"))
		| Transpose(s)				-> let typ = type_of_identifier s symbols in
										(match typ with
											Datatype(Matrix(d, r, c)) -> STranspose(s, Datatype(Matrix(d, c, r)))
											| _ -> raise(Failure"Cannot operate on nonmatrix"))
		| Trace(s)					-> let typ = type_of_identifier s symbols in
										(match typ with 
										Datatype(Matrix(d, r, c)) -> STrace(s, Datatype(Matrix(d,r,c)))
											| _ -> raise(Failure"Cannot operator on nonmatrix"))
		| SubMatrix(s,e1,e2,e3,e4)  -> let se1 = sexpr symbols e1 in
										let se2 = sexpr symbols e2 in
										let se3 = sexpr symbols e3 in
										let se4 = sexpr symbols e4 in
										let typ = type_of_identifier s symbols in
											match typ with 
											Datatype(Matrix(d,r,c)) ->  SSubMatrix(s, se1, se2, se3, se4, Datatype(Matrix(d,r,c)))
											| _ -> raise(Failure"Cannot operator on nonmatrix")
	in

	let check_bool_expr symbols e = 
		if (Sast.get_sexpr_type (sexpr symbols e)) = Datatype(Bool) 
		|| (Sast.get_sexpr_type (sexpr symbols e)) = Datatype(Int)
	    then (sexpr symbols e)
	    else raise (Failure "Expects Boolean expression") 
	in

	(* Verify a statement or throw an exception *)
	let rec sstmt symbols = function
		Block(stmt_list) 		-> SBlock(stmt_list_to_sstmt_list symbols stmt_list)
		| Expr(e) 				-> SExpr(sexpr symbols e)
		| If(e, s1, s2) 		-> SIf((check_bool_expr symbols e), (sstmt symbols s1), (sstmt symbols s2))
		| For(e1, e2, e3, s) 	-> SFor((sexpr symbols e1), (check_bool_expr symbols e2), (sexpr symbols e3), (sstmt symbols s))
		| While(e, s)			-> SWhile((check_bool_expr symbols e), (sstmt symbols s))
		| Return(e)				-> SReturn(sexpr symbols e)

	and stmt_list_to_sstmt_list symbols stmt_list = List.map (sstmt symbols) stmt_list
	in


	let func_to_sfunc func =
		{
			sfname 			= func.fname;
			styp 			= func.typ;
			sformals 		= func.formals;
			slocals 		= func.locals;
			sbody 			= (stmt_list_to_sstmt_list (func_to_symbols func) func.body);
		}
	in
	

	(* check functions *)
	ignore(List.iter check_function functions);

	(* convert func to sfunc *)
	let sfuncs = List.map func_to_sfunc functions 
	in	(globals, sfuncs)