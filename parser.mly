%{
	open Minilang
        open Printf 
	open Lexing

	let gen_error message s = 
		raise (SyntaxError 
			(sprintf "Parser - %s, near line %d column %d" message s.pos_lnum (s.pos_cnum - s.pos_bol + 1)))
%}

%token <string> Id LString
%token <bool> LBool
%token <int> LInt
%token <float> LFloat
%token <Minilang.datatype> EType
%token <Minilang.operator> ERelational EComparison EMultiplicative
%token EOF TLBrace TRBrace TSemi TEquals TVar TColon TRead TPrint TWhile TIf TElse TLParen TRParen TOr TAnd TPlus TMinus TNot

%start main
%type <Minilang.ast> main
%%

/* --- Blocks --- */
/* Programs are implicitly a block */

main:
	| statements EOF			{Block $1}
block:
	| TLBrace statements block_catch_runaway {Block $2}
block_catch_runaway:
	| TRBrace				{}
	| EOF					{gen_error "End of file in open block" $startpos}

/* --- Statements --- */
statement:
	| declaration_statement stmt_end	{$1}
	| assignment_statement stmt_end		{$1}
	| read_statement stmt_end		{$1}
	| print_statement stmt_end		{$1}
	| while_statement			{$1}
	| if_statement				{$1}
	| TSemi					{gen_error "Extraneous ';'" $startpos}
stmt_end:
	| TSemi					{None}
	| guess					{gen_error "Improperly terminated statement" $startpos}
statements:
	| statement statements			{$1::$2}
	| 					{[]}


/* .. Single line staZZtements .. */

/* Declaration */
declaration_statement:
	| declaration				{Declare((fst $1), (snd $1), None)}
	| declaration TEquals expr		{Declare((fst $1), (snd $1), (Some $3))}
declaration:
	| TVar Id TColon EType			{($2, $4)}
	| TVar error				{gen_error "An identifier was expected" $startpos($2)}
	| TVar Id TColon error			{gen_error "A valid data type was expected" $startpos($4)}

/* Assignment */
assignment_statement:
	| Id TEquals expr			{Assign($1,$3)}
	
/* Pseudofunctions */
read_statement:
	| TRead TLParen Id TRParen 		{Read $3}
	| TRead error				{gen_error "A variable is required as argument to read" $endpos($1)}
	| TRead TLParen error			{gen_error "A variable was expected" $startpos($3)}

print_statement:
	| TPrint paren_expr 			{Print $2}

/* .. Control structures .. */


/* While */
while_statement:
	| TWhile paren_expr block		{While($2, $3)}
	| TWhile paren_expr guess		{gen_error "A block was expected after while condition" $endpos($2)}

/* If */
/* It's kind of wrong to say that the else if is a block, but good enough for now */

if_statement:
	| TIf paren_expr block if_tail		{If($2, $3, $4)}
	| TIf paren_expr guess			{gen_error "A block was expected after if condition" $endpos($2)}
if_tail:
	| TElse if_statement			{Some (Block [$2])}
	| TElse block 				{Some $2}
	| TElse block error			{gen_error "No clause may follow the else clause" $startpos($3)}
	| TElse error				{gen_error "A block or if statement was expected" $endpos($1) }
	| error					{gen_error "Unexpected statement after if" $startpos($1) }
	|					{None}
/* --- Expressions --- */

paren_expr:
	| subexpr				{$1}

subexpr:
	| TLParen expr TRParen 			{$2}
	(*| TLParen error				{gen_error "Unbalanced parentheses" $startpos} *)
	| TLParen TRParen			{gen_error "Empty sub-expression" $startpos($1)}
expr:
	| expr750 TNot expr700			{gen_error "Unexpected '!' (unary operator only)" $startpos($2)}
	| expr750				{$1}

/* .. Expressions .. */
/* The numbers indicate the priority level */

expr750: 
	| expr750 TOr expr700 			{Op2(OR,$1,$3)}
	| expr700				{$1}
expr700: 
	| expr700 TAnd expr600 			{Op2(AND,$1,$3)}
	| expr600				{$1}
expr600: 
	| expr600 EComparison expr500 		{Op2($2,$1,$3)}
	| expr500				{$1}
expr500: 
	| expr500 ERelational expr400 		{Op2($2,$1,$3)}
	| expr400				{$1}
expr400: 
	| expr400 TPlus expr300 		{Op2(ADD,$1,$3)}
	| expr400 TMinus expr300 		{Op2(SUB,$1,$3)}
	| expr300				{$1}
expr300: 
	| expr300 EMultiplicative expr200 	{Op2($2,$1,$3)}
	| expr200				{$1}
expr200:
	| TMinus expr200 			{Op1(SUB,$2)}
	| TNot expr200				{Op1(NOT,$2)}
/* I think we can only do this properly once rid of the binaries */
	| binop					{gen_error "Insufficient arguments to binary operator" $startpos($1)} 
	| expr100				{$1}
expr100:
	| subexpr				{$1}
	| value					{$1}
	| error					{gen_error "An expression was expected" $startpos}
binop: 
	TOr | TAnd | EComparison | ERelational | TPlus | EMultiplicative {None}


/* .. Values .. */
value:
	| Id					{Var $1}
	| literal				{Val $1}
literal:
	| LBool					{Bool $1}
	| LInt					{Int $1}
	| LFloat				{Float $1}
	| LString				{String $1}


/* .. Guess the mistake .. */

guess:
/*	| TRParen error			{gen_error "Unexpected ')', expression already closed" $startpos}
	| error TRParen			{gen_error "Unexpeced ')', expression already closed" $startpos($2)} */
	| error  			{ true }
