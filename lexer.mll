{
open Printf
open Lexing
open Parser
open Minilang

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }


let gen_error lexbuf =
	let lexeme = Lexing.lexeme lexbuf in
	let pos = Lexing.lexeme_start_p lexbuf in
		sprintf "Lexer - Unexpected symbol (%s) at line %d, column %d" lexeme pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

}


let ws = [' ' '\t']+

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_'  '0'-'9']*

(* As per the prof's assignment just to be safe *)
let string  = ['a'-'z' 'A'-'Z' '0'-'9'
	'`' '~' '!' '@' '#' '$' '%' '^'  '&' '*' '(' ')' '-' '_' '=' '+'
	'[' ']' '{' '}' '|' ';' ':'  '\'' ',' '<' '.' '>' '/' '?' ' ']

let int = (['1'-'9'] ['0'-'9']* | ['0'])
let frac = ['.'] ['0'-'9']+
let float = int frac?
let padded_number = ['0']+ (float | int)

rule lex_main = parse
	| "//" [^'\n']* 	{ lex_main lexbuf }
	| ws				{ lex_main lexbuf }
	| "\n"				{ next_line lexbuf; lex_main lexbuf}

	| "true"			{ LBool true }
	| "false"			{ LBool false }
	| padded_number 		{raise (SyntaxError "Numeric literals must not be padded with leading zeros")}
	| int as i			{ LInt (int_of_string i) }
	| float as f			{ LFloat (float_of_string f) }
	| '"'				{ LString (lex_string (Buffer.create 32) lexbuf ) }

	| "var"				{TVar}
	| "read"			{TRead}
	| "print"			{TPrint}
	| "while"			{TWhile}
	| "if"				{TIf}
	| "else"			{TElse}
	| "boolean"			{ EType BOOL }
	| "int"				{ EType INT }
	| "float"			{ EType FLOAT }
	| "string"			{ EType STRING }

	| "("				{TLParen}
	| ")"				{TRParen}
	| "{"				{TLBrace}
	| "}"				{TRBrace}
	| ";"				{TSemi}
	| ":"				{TColon}
	| "="				{TEquals}

	| "&&"				{TAnd}
	| "||"				{TOr}
	| "=="				{ ERelational EQ }
	| "!="				{ ERelational NEQ }
	| "<"				{ EComparison LT }
	| "<="				{ EComparison LEQ }
	| ">"				{ EComparison GT }
	| "+"				{TPlus}
	| "-"				{TMinus}
	| ">="				{ EComparison GEQ }
	| "*"				{ EMultiplicative MUL }
	| "/"				{ EMultiplicative DIV }
	| "!"				{TNot}

	| ident as id			{ Id id }
	| eof				{EOF}
	| _ 				{ raise (SyntaxError (gen_error lexbuf)) }
and lex_string b = parse
	| '"'				{ Buffer.contents b }
	| '\\'				{ lex_escape b lexbuf }
	| string* as str		{ Buffer.add_string b str; lex_string b lexbuf }
	| eof 				{ raise (SyntaxError "Lexer - End of file in quoted string") }
	| _ 				{ raise (SyntaxError (gen_error lexbuf)) }
and lex_escape b = parse
	| 'a'				{ Buffer.add_char b '\007'; lex_string b lexbuf }
	| 'b'				{ Buffer.add_char b '\008'; lex_string b lexbuf }
	| 'f'				{ Buffer.add_char b '\012'; lex_string b lexbuf }
	| '"'				{ Buffer.add_char b '"'; lex_string b lexbuf } 
	| 'n'				{ Buffer.add_char b '\n'; lex_string b lexbuf }
	| 'r'				{ Buffer.add_char b '\r'; lex_string b lexbuf }
	| 't'				{ Buffer.add_char b '\t'; lex_string b lexbuf }
	| '\\'				{ Buffer.add_char b '\\'; lex_string b lexbuf }
	| 'v'				{ Buffer.add_char b '\011'; lex_string b lexbuf }
	| eof				{ raise (SyntaxError "Lexer - End of file in escape sequence") }
	| _				{ raise (SyntaxError "Lexer - Invalid escape character") }

