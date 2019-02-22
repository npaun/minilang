exception SyntaxError of string

type datatype = BOOL | INT | FLOAT | STRING
type operator = OR | AND | EQ | NEQ | LT | LEQ | GT | GEQ | ADD | SUB | MUL | DIV |  NOT

type ast = block
and block = Block of statement list
and statement = 
	| Assign of symbol * expression
	| Declare of symbol * datatype * expression option
	| Print of expression
	| Read of symbol
	| If of expression * block * block option
	| While of expression * block
and expression = 
	| Op2 of operator * expression * expression
	| Op1 of operator * expression
	| Var of symbol
	| Val of value
and value =
	| Bool of bool 		(* type 1 *)
	| Int of int 		(* type 2 *)
	| Float of float	(* type 3 *)
	| String of string	(* type 4 *)
and symbol = string


