open Printf

let load_text () = 
	let cin = if Array.length Sys.argv > 2
			then open_in Sys.argv.(2)
			else stdin
	in Lexing.from_channel cin

let with_error_handling fn ok =
	try (
		fn ();
		if ok then printf "OK\n"
	) with 
	| Minilang.SyntaxError message 
	| Symbol.SymbolError message
	| Typecheck.TypeError message -> (
		fprintf stderr "Error: %s\n" message;
		 exit 1
	)

let parse lexbuf = Parser.main Lexer.lex_main lexbuf

let main () = 
	let lexbuf = load_text() in
		match Sys.argv.(1) with
		| "scan" -> with_error_handling (fun () -> Pprint.lexbuf_as_string Lexer.lex_main lexbuf) true
		| "tokens" -> with_error_handling (fun () -> printf "%s\n" (Pprint.lexbuf_as_string Lexer.lex_main lexbuf)) false
		| "parse" -> with_error_handling (fun () -> Parser.main Lexer.lex_main lexbuf) true 
		| "pretty" -> with_error_handling (fun () -> parse lexbuf |> Pprint.pretty |> printf "%s\n") false
		| "symbol" -> with_error_handling (fun () -> parse lexbuf |> Symbol.gen_ast Symbol.verbose) false
		| "typecheck" -> with_error_handling (fun () -> parse lexbuf |> Typecheck.type_ast) true
		| _ -> printf "Go away\n"

let _ = main ()
