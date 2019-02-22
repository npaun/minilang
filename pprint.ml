open Parser
open Minilang
open Printf

let type_as_string = function
| BOOL -> "boolean"
| INT -> "int"
| FLOAT -> "float"
| STRING -> "string"

let operator_as_string = function
| EQ -> "=="
| NEQ -> "!="
| LT -> "<"
| LEQ -> "<="
| GT -> ">"
| GEQ -> ">="
| MUL -> "*"
| DIV -> "/"
| ADD -> "+"
| SUB -> "-"
| NOT -> "!"
| OR -> "||"
| AND -> "&&"

let value_as_string = function
| Bool v -> sprintf "%B" v
| Int v -> sprintf "%d" v
| Float v -> sprintf "%f" v
| String v -> sprintf "\"%s\"" (String.escaped v) (* wrong, but I am tired *)

(* Pretty printer *)
type chunk = Line of string | Protect of string | Indent of chunk list 

let compositor prog = 
	let rec aux l0 l1 acc prog = 
		let tabs = String.make l0 '\t' in
		let tsr l = sprintf "%s%s" tabs l in
		let ts l = sprintf "%s%s\n" tabs l in
		match prog with	
		| [] -> acc
		| (Line l)::t -> aux l1 l1 ((ts l)::acc) t
		| (Indent i)::t -> aux l1 l1 (aux (l1 + 1) (l1 + 1) acc i) t
		| (Protect p)::t -> aux 0 l1 ((tsr p)::acc) t 
		
	in
	aux 0 0 [] prog |> List.rev |> String.concat ""

let rec pp_block = function Block block -> [
	Line "{";
	Indent (pp_statements [] block);
	Line "}"]
and pp_statements acc = function
| [] -> acc
| h::t -> pp_statements (acc @ (pp_statement h)) t
and pp_statement = function
| Assign(var,expr) -> 
	Line (sprintf "%s = %s;" var (pp_expr expr)) :: []
| Declare(var,decltype,None) -> 
	Line (sprintf "var %s:%s;" var (type_as_string decltype)) :: []
| Declare(var,decltype,Some expr) -> 
	Line (sprintf "var %s:%s = %s;" var (type_as_string decltype) (pp_expr expr)) :: []
| Print(expr) ->
	Line (sprintf "print(%s);" (pp_expr expr)) :: []
| Read(var) ->
	Line (sprintf "read(%s);" var) :: []
| While(cond, body) -> pp_control "while" cond body
| If(cond, t, None) -> pp_control "if" cond t
| If(cond, Block t, Some Block [If(cond',t',f')]) -> 
	[Protect (sprintf "if (%s) " (pp_expr cond));
	Line "{";
	Indent (pp_statements [] t);
	Protect "} else "] @ (pp_statement (If(cond', t', f')))
| If(cond, Block t, Some f) -> 
	[Protect (sprintf "if (%s) " (pp_expr cond));
	 Line "{";
	 Indent (pp_statements [] t);
	 Protect "} else "] @ pp_block f
and pp_control name cond body = 
	Protect (sprintf "%s (%s) " name (pp_expr cond)) :: pp_block body
and pp_expr = function
| Op2(op,(Val _ as a),(Val _ as b)) | Op2(op, (Var _ as a),(Var _ as b))
| Op2(op,(Val _ as a),(Var _ as b)) | Op2(op, (Var _ as a),(Val _ as b))
  -> sprintf "%s %s %s" (pp_expr a) (operator_as_string op) (pp_expr b)
| Op2(op,(Val _ as a),b) | Op2(op, (Var _ as a),b)  -> sprintf "%s %s (%s)" (pp_expr a) (operator_as_string op) (pp_expr b)
| Op2(op,a,(Val _ as b)) | Op2(op,a,(Var _ as b))  -> sprintf "(%s) %s %s" (pp_expr a) (operator_as_string op) (pp_expr b)
(* This is still too many parens if I were to consider precedence *)
| Op2(op,a,b) -> sprintf "(%s) %s (%s)" (pp_expr a) (operator_as_string op) (pp_expr b)
| Op1(op,(Val _ as a)) | Op1(op, (Var _ as a))  -> sprintf "%s%s" (operator_as_string op) (pp_expr a)
| Op1(op,a) -> sprintf "%s(%s)" (operator_as_string op) (pp_expr a)
| Var v -> sprintf "%s" v
| Val v -> value_as_string v

let pretty = function Block block -> pp_statements [] block |> compositor 
	
(* Token dump *)

let token_as_string = function
| LBool v -> sprintf "lBool %B" v
| LInt v -> sprintf "lInt %d" v
| LFloat v -> sprintf "lFloat %f" v
| LString v -> sprintf "lString \"%s\"" v
| TVar -> "tVar"
| TRead -> "tRead"
| TPrint -> "tPrint"
| TWhile -> "tWhile"
| TIf -> "tIf"
| TElse -> "tElse"
| EType v -> sprintf "eType:%s" (type_as_string v)
| TLParen -> "tLParen"
| TRParen -> "tRParen"
| TLBrace -> "tLBrace"
| TRBrace -> "tRBrace"
| TSemi -> "tSemi"
| TColon -> "tColon"
| TEquals -> "tEquals"
| TAnd -> "tAnd"
| TOr -> "tOr"
| ERelational op -> sprintf "eRelational:%s" (operator_as_string op)
| EComparison op -> sprintf "eComparison:%s" (operator_as_string op)
| EMultiplicative op -> sprintf "eMultiplicative:%s" (operator_as_string op)
| TPlus -> "tPlus"
| TMinus -> "tMinus"
| TNot -> "tNot"
| Id v -> sprintf "Id %s" v
| EOF -> ""

let lexbuf_as_string lexfun buf = 
	let rec collect acc =
		match (lexfun buf) with
		| EOF -> List.rev acc
		| t -> collect ((token_as_string t)::acc)
	in String.concat "\n" (collect [])
