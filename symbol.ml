open Printf
open Minilang

type sym_entry = { decltype:datatype}
type sym_table = { debug:string; map:(string,sym_entry) Hashtbl.t; parent:sym_table option}
exception SymbolError of string

let new_table () = Hashtbl.create 8

let search1 syms var = Hashtbl.find_opt syms.map var
let rec search syms var = 
	let res1 = search1 syms var in
		match (res1, syms.parent) with
		| (Some entry,_) -> Some entry
		| (None, Some parent) -> search parent var
		| _ -> None
let ensure_defined syms var = 
	match search syms var with
	| Some entry -> ()
	| None -> raise (SymbolError (sprintf "Undeclared identifier %s" var))

let verbose line = printf "%s" line
let silent _ = ()

let rec gen_ast print block =
	let globalsyms = { map = new_table (); parent = None; debug = "_" } in
		gen_block print globalsyms [globalsyms] "GLOBAL" block  |> List.rev
and gen_block print syms acc debug = function 
| Block block -> (
		print (sprintf "(scope %s\n" debug);
		let syms' = { map = new_table () ; parent = Some syms; debug} in
			 let acc' = List.fold_left (gen_statement print syms') (syms'::acc) block in 
				print (sprintf ")\n"); acc'
	)
and gen_statement print syms acc = function
	| Declare(var,decltype,initialization) -> gen_declare print syms var decltype initialization; acc 
	| Assign(var,expr) -> ensure_defined syms var; ensure_expr syms expr; acc
	| Print(expr) -> ensure_expr syms expr; acc
	| Read(var) -> ensure_defined syms var; acc
	| While(cond,block) -> ensure_expr syms cond; (gen_block print syms acc "WHILE-BODY" block)
	| If(cond,t,None) -> ensure_expr syms cond;  (gen_block print syms acc "IF-TRUE" t)
	| If(cond,t,Some f) -> ensure_expr syms cond;
		let symst = gen_block print syms acc "IF-TRUE" t  in
		(* This design seems to generate too many scopes for the else clauses, but they're pretty harmless *)
		gen_block print syms symst "IF-FALSE" f
and gen_declare print syms var decltype initialization = 
	(match initialization with
	 | Some expr -> ensure_expr syms expr
	 | _ -> ()
	);
	match (search1 syms var) with
		| Some entry -> raise (SymbolError (sprintf "Attempt to redefine %s" var)) 
		| None -> (
				print (sprintf "	%s %s\n" var (Pprint.type_as_string decltype));
				Hashtbl.add syms.map var {decltype = decltype};
				()
		)
and ensure_expr syms = function
	| Op2(_,a,b) -> ensure_expr syms a; ensure_expr syms b
	| Op1(_,a) -> ensure_expr syms a
	| Var v -> ensure_defined syms v
	| Val _ -> ()

