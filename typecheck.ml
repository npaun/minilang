open Minilang
open Printf
open Pprint

exception TypeError of string


let debug line = printf "%s" line
 let debug _ = () 

let assignable lhs rhs name expr = match (lhs, rhs) with
| (FLOAT,INT) -> true
| (t1,t2) when t1=t2 -> true
| (u,v) -> raise (TypeError (sprintf "Variable '%s' is of type %s but expression %s is of type %s" name (type_as_string u) (pp_expr expr) (type_as_string v)))


let require_type name signature exp expr act yield = 
	if exp = act then yield
	else raise (TypeError (sprintf "'%s' is of type %s but argument %s has type %s" name signature (pp_expr expr) (type_as_string act)))

let require_arith ta tb = 
	let type_message op arg act =
		raise (TypeError (sprintf "'%s' is of type int -> int -> int or 'a -> 'a -> float where 'a = float | int\n\t, but argument %s has type %s"
			(operator_as_string op) (pp_expr arg) (type_as_string act))) 
	in function 
		| Op2(op,a,b) -> ( 
			match (ta,tb) with
			| (INT,INT) -> INT
			| (FLOAT,FLOAT) | (INT,FLOAT) | (FLOAT,INT) -> FLOAT
			| (FLOAT,_) | (INT,_) -> type_message op b tb
			| (_,_) -> type_message op a ta
		 ) 
		| _ -> raise (TypeError "mistake in type checker")

let handle_add ta tb expr = 
	if (ta,tb) = (STRING,STRING) then STRING
	else require_arith ta tb expr (* type signature in require_arith errors isn't complete for + *)

let matching_types expr typeof_a typeof_b yield = 
	if typeof_a = typeof_b then yield
	else match expr with  
	| Op2(op,_,_) -> raise (TypeError (sprintf "'%s' is of type 'a -> 'a -> %s but arguments are not of the same type" (operator_as_string op) (type_as_string yield))) 
	| _ -> raise (TypeError "mistake in type checker")

let type_of_val = function 
	| Float _ -> FLOAT
	| Int _ -> INT
	| String _ -> STRING
	| Bool _ -> BOOL

let typeof syms var = match Symbol.search syms var with
	| Some entry -> entry.decltype
	| None -> raise (TypeError "symbol table was generated improperly?")

let rec type_ast ast = let scopes = Symbol.gen_ast Symbol.silent ast in type_block (List.tl scopes) ast
and type_block scopes = function Block block ->
	sprintf "Found scope %s\n" (List.hd scopes).debug |> debug; 
	List.fold_left type_statement scopes block 
and type_statement scopes =
	let syms::rest = scopes in 
	function 
	| Declare(_,_,None) | Read(_) -> scopes (* any type is fine for the checker *)
	| Print(expr) -> 
			let _ = type_expr syms expr in
				scopes	
	| Declare(var,decltype,Some initialization) -> 
		let typeof_rhs = type_expr syms initialization in
			sprintf "%s is of %s, initialized to %s\n" var (type_as_string decltype) (type_as_string typeof_rhs) |> debug;
			let _ = assignable decltype typeof_rhs var initialization in
				scopes
	| Assign(var,expr) ->
		let decltype = typeof syms var in
		let typeof_rhs = type_expr syms expr in
			sprintf "%s is of %s, set to %s\n" var (type_as_string decltype) (type_as_string typeof_rhs) |> debug;
			let _ = assignable decltype typeof_rhs var expr in
				scopes
	| While(cond, block) ->
		require_type "while condition" "boolean" BOOL cond (type_expr syms cond) ();
		type_block rest block 
	| If(cond, t, f) ->
		require_type "if condition" "boolean" BOOL cond (type_expr syms cond) ();
		let rest' = type_block rest t in
			( match f with
		 	  | Some fblock -> type_block rest' fblock
			  | None -> rest'
			)
and type_expr syms = function
	| Val l -> type_of_val l
	| Var v -> typeof syms v
	| Op1(NOT,a) as expr -> require_type "!" "boolean -> boolean" BOOL expr (type_expr syms a) BOOL
	| Op1(SUB,a) -> ( match type_expr syms a with
			  | INT -> INT
			  | FLOAT -> FLOAT
			  | other -> raise (TypeError (sprintf "'-' (unary) is of type 'a -> 'a where 'a = float int, but %s is of type %s" (pp_expr a) (type_as_string other)))
			) 		
	| Op1(_,_) -> raise (TypeError "Something has gone wrong")
	| Op2(ADD,a,b) as expr -> handle_add (type_expr syms a) (type_expr syms b) expr 
	| Op2(AND as op,a,b) | Op2(OR as op,a,b) as expr -> ( 
		require_type (operator_as_string op) "boolean -> boolean -> boolean" BOOL expr (type_expr syms a) ();
		require_type (operator_as_string op) "boolean -> boolean -> boolean" BOOL expr (type_expr syms b) (); 
		BOOL
	)
	| Op2(EQ,a,b) | Op2(NEQ,a,b) 
	| Op2(LT,a,b) | Op2(LEQ,a,b)
	| Op2(GT,a,b) | Op2(GEQ,a,b) as expr -> matching_types expr (type_expr syms a) (type_expr syms b) BOOL	
	| Op2(SUB,a,b) | Op2(MUL,a,b) | Op2(DIV,a,b) as expr -> require_arith (type_expr syms a) (type_expr syms b) expr
	| Op2(_,_,_) -> raise (TypeError "Something has gone wrong")
and types a b = (type_expr a, type_expr b)


