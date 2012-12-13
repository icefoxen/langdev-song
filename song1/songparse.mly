%{
(* songparse.mly
   A parser for Song.
   Yay.

   Simon Heath
   18/2/2004
*)



%}


%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COLON COMMA PERIOD HASH AT
%token TRY RAISE WITH FINALLY
%token BREAK RET CONTINUE TYPE STRUCT
%token IF ELIF ELSE CASE DEFAULT GOTO 
%token FOR FROM TO BY WHILE UNTIL PASS
%token NULL NONE ASM INDENT DEDENT ENDLN SIZEOF
%token USE IMPORT PACKAGE EXPORT
%token <string> STRING ID
%token <char> CHAR
%token <Int64.t> INT
/* We dun need 32 or 64 bit floats here 'cause nobody's going to WRITE one.
   I hope. */
%token <float> FLOAT
%token ASSIGN ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV ASSIGNMOD ASSIGNAND
%token ASSIGNOR ASSIGNXOR SWAP
%token EQ GT LT GTE LTE NEQ SEQ SNEQ
%token AND OR XOR NOT BAND BOR BXOR BNOT
%token PTR ADD SUB MUL DIV MOD INC DEC
%token SHL SHR
%token CLASS METHOD METHODCALL SELF SUPER
%token GLOBAL CONST

%token UINT8 INT8
%token UINT16 INT16
%token UINT32 INT32 WORD
%token UINT64 INT64
%token FLOAT32 FLOAT64
%token FUNC
/* STRING is a VALUE, STR is a TYPE DEC!! */
%token STR

/* Prescedence and associativity... later is higher prescedence */
%left     ASSIGN ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV ASSIGNMOD ASSIGNAND ASSIGNOR ASSIGNXOR SWAP
%nonassoc EQ GT LT GTE LTE NEQ SEQ SNEQ
%nonassoc AND OR XOR
%right    NOT
%nonassoc SHL SHR
%nonassoc BAND BOR BXOR
%right    BNOT
%left     PERIOD
%right    PTR
%nonassoc ADD SUB
%nonassoc MUL DIV MOD
%right    SIZEOF
%right    INC DEC


%type <unit> main
%start main

%%
main: 
	defs
	  	{}
	;


/* EXPRESSIONS */
/* Unneeded, since expr is a subtype of stms */
exprs:
	  expr ENDLN
		{}
	| expr ENDLN exprs
		{}
	;


expr:
	  value
	  	{}
	| funcall
		{}
	| arith
		{}
	| logic
		{}
	| binlogic
		{}
	| addrref
		{}
	| LPAREN expr RPAREN
		{}
	| LPAREN expr error
		{ ErrorReport.error "Unclosed paren?";
		  raise Parsing.Parse_error }
	;


/* EXPRESSIONS - SUBEXPRS */
number:
	  INT
	  	{}
	| FLOAT
		{}
	| CHAR
		{}
	;

value:
	  number
	  	{}
	| lexpr
		{}
	| NULL
		{}
	| STRING
		{}
	;

/*
binop:
	  ADD
	  	{}
	| SUB 
		{}
	| DIV
		{}
	| MUL
		{}
	| MOD
		{}
	| AND
		{}
	| OR
		{}
	| XOR
		{}
	| EQ
		{}
	| NEQ
		{}
	| SEQ
		{}
	| SNEQ
		{}
	| BAND
		{}
	| BOR
		{}
	| BXOR
		{}
	| BNOT
		{}
	| SHL
		{}
	| SHR
		{}
	;
*/

arith:
	  expr ADD expr
	  	{}
	| expr SUB expr
		{}
	| expr DIV expr
		{}
	| expr MUL expr
		{}
	| expr MOD expr
		{} 
	| expr ADD error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr SUB error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr DIV error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr MUL error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr MOD error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;
	
logic:
	  expr AND expr
	  	{}
	| expr OR expr
		{}
	| expr XOR expr
		{}
	| NOT expr
		{}
	| expr EQ expr
		{}
	| expr SEQ expr
		{}
	| expr NEQ expr
		{}
	| expr SNEQ expr
		{}
	| expr AND error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr OR error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr XOR error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| NOT error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr EQ error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr SEQ error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr NEQ error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr SNEQ error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;
	
binlogic:
	  expr BAND expr
		{} 
	| expr BOR expr
		{}
	| expr BXOR expr
		{}
	| BNOT expr
		{}
	| expr SHL expr
		{}
	| expr SHR expr
		{}
	| expr BAND error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr BOR error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr BXOR error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| BNOT error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr SHL error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	| expr SHR error
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;



/* EXPRESSIONS - COMPOUND REFS */
/* Meh.  forget this.  Referring to structs and arrays are too close together
   to tell apart.  It's up to the semantic analysis to tell them apart.
   Shouldn't be too hard.  Type-checking, after all.
structref:
	  expr PERIOD ID
	  	{}
	;
*/

lexpr:
	  ID 
	  	{}
	| arrayref
		{}
	| ptrref
		{}
	;

arrayref:
	  expr PERIOD expr
	  	{ print_endline "Arrayref parsed" }
	| expr PERIOD error 
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;

ptrref:
	  PTR expr
	  	{}
	| PTR error 
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;
addrref:
	  AND expr
	  	{}
	| AND error 
		{ ErrorReport.error "expr expected?";
		  raise Parsing.Parse_error }
	;



/* DEFS - FUNCTIONS */
argitem:
	  expr
	  	{}
	| ID COLON expr
		{}
	| ID COLON error
		{ ErrorReport.error "Expression expected?";
		  raise Parsing.Parse_error }
	;
arglst:
	  argitem
	  	{}
	| argitem COMMA argitem
		{} 
	| /* NOTHING */
		{}
	| argitem COMMA error 
		{ ErrorReport.error "Arg expected?";
		  raise Parsing.Parse_error }
	| argitem error argitem
		{ ErrorReport.error "Comma expected?";
		  raise Parsing.Parse_error }
	;

funcall:

	  ID LPAREN arglst RPAREN
	  	{ print_endline "Funcall parsed" }
	| ID LBRACE typedec RBRACE LPAREN arglst RPAREN 
		{print_endline "Funcall w/ type param parsed" }
	| ID LBRACE error
		{ ErrorReport.error "} expected?";
		  raise Parsing.Parse_error }
	| ID LPAREN error
		{ ErrorReport.error ") expected?";
		  raise Parsing.Parse_error }
	;
	

arraydec:	
	  LBRACK typedec RBRACK
		{}
	| LBRACK typedec error 
		{ ErrorReport.error "} expected?";
		  raise Parsing.Parse_error }
	| LBRACK error 
		{ ErrorReport.error "typedec expected?";
		  raise Parsing.Parse_error }
	;

ptrdec:
	  PTR typedec
	  	{}
	| PTR error 
		{ ErrorReport.error "typedec expected?";
		  raise Parsing.Parse_error }
	;




/* STATEMENTS */

block:
	  ENDLN INDENT stms DEDENT
		{ print_endline "Block parsed" }
	| stm
		{ print_endline "Block-statement parsed" }
	| INDENT stms error
		{ ErrorReport.error "Dedent expected?";
		  raise Parsing.Parse_error }
	;


stm:
	  exprs
		{}
	| vardec
		{}
	| ENDLN
		{}
	| assignstm
		{}
	| ifstm
		{}
	| forstm
		{}
	| fromstm
		{}
	| whilestm
		{}
	| untilstm
		{}
	| trystm
		{}
	| gotostm
		{}
	| asmstm
		{}
	/*| casestm
		{} */
	| retstm
		{}
	| gotolabel
		{}
	| PASS
		{}
	;

stms:
	  stm
	  	{}
	| stm ENDLN stms
		{}
	| error
		{ ErrorReport.error "Statements expected?";
		  raise Parsing.Parse_error }
	;






/* STATEMENTS - VARIABLES */
typedec:
	  UINT8
	  	{}
	| INT8
		{ }
	| UINT16
		{ }
	| INT16
		{ }
	| UINT32
		{ }
	| INT32
		{ }
	| UINT64
		{ }
	| INT64
		{ }
	| FLOAT32
		{ }
	| FLOAT64
		{ }
	| NONE 
		{ }
	| STR
		{ }
	| FUNC
		{ }
	| arraydec
		{ }
	| ptrdec
		{ }
	| ID
		{ }
	;


vardec:
	/* foo int */
	  ID typedec
	  	{ print_endline "Simple vardec parsed" }
	/* foo int <- 10 */
	| ID typedec ASSIGN expr
		{print_endline "Vardec w/ assignment parsed" }
	/* foo struct@ (static var) */
	| ID typedec AT
		{print_endline "Static vardec parsed" }
	/* foo struct@ <- blah */
	| ID typedec AT ASSIGN expr
		{print_endline "Static vardec w/ assignment parsed" }
	/* foo [int]@3 (array on stack --static var) */
	| ID typedec AT INT
		{print_endline "Static array vardec parsed" }
	;



/* STATEMENTS - CONTROL AND SUCH */

retstm:
	  RET ENDLN
	  	{}
	| RET expr ENDLN
		{}
	;


gotolabel:
	  ID HASH
	  	{}
	;

elif:
	  ELIF expr COLON block
		{ print_endline "elif parsed" }
	| ELIF expr error 
		{ ErrorReport.error "Colon expected?";
		  raise Parsing.Parse_error }
	| ELIF error 
		{ ErrorReport.error "Expression expected?";
		  raise Parsing.Parse_error }
	;

eliflst:
	  elif
		{}
	| elif eliflst
		{}
	;

/* XXX: Ambiguity galore; shift-reduces on the ELSE and eliflst.
   The classic dangling-else problem.  No worries.  */
ifstm:
	  IF expr COLON block ELSE COLON block
	  	{ print_endline "If-else parsed" }
	| IF expr COLON  block eliflst ELSE COLON block
		{ print_endline "If-elif-else parsed" }
	| IF expr COLON block
	  	{ print_endline "If parsed" } 
	| IF expr error 
		{ ErrorReport.error "Colon expected?";
		  raise Parsing.Parse_error }
	| IF error
		{ ErrorReport.error "Expr expected?";
		  raise Parsing.Parse_error }
	;

loopstm:
	  stm
		{}
	| BREAK
		{}
	| CONTINUE
		{}
	;

loopstms:
	  loopstm
		{}
	| loopstm ENDLN loopstms
		{}
	;

loopblock: 
	  ENDLN INDENT loopstms DEDENT
		{ print_endline "Loopblock parsed" }
	| loopstm
		{}
	| INDENT loopstms error
		{ ErrorReport.error "Dedent expected?";
		  raise Parsing.Parse_error }

forstm:
	  FOR stm ENDLN stm ENDLN stm COLON loopblock
	  	{print_endline "For stm parsed" }
	;
	
fromstm:  
	  FROM ID ASSIGN expr TO INT COLON loopblock
	  	{print_endline "From stm parsed" }
	| FROM ID ASSIGN expr TO INT BY INT COLON loopblock
		{ print_endline "From-by stm parsed" }
	;

whilestm:
	  WHILE expr COLON loopblock
	  	{print_endline "While parsed" }
	| WHILE error
		{ ErrorReport.error "Expression expected in while?";
		  raise Parsing.Parse_error }
	| WHILE expr error
		{ ErrorReport.error "Colon expected in while?";
		  raise Parsing.Parse_error }
	| WHILE expr COLON error
		{ ErrorReport.error "Block expected in while?";
		  raise Parsing.Parse_error }
	;

untilstm:
	  UNTIL expr COLON loopblock
	  	{print_endline "Until parsed" }
	;

/* Causes a shift-reduce conflict with the last finally.  No worries.
   Similar to dangling-else.  */
trystm:
	  TRY COLON block WITH ID COLON block
	  	{print_endline "Try-with parsed"} 
	| TRY COLON block FINALLY COLON block 
		{print_endline "Try-finally parsed" }
	| TRY COLON block WITH ID COLON block FINALLY COLON block
		{print_endline "Try-with-finally parsed" }
	;

gotostm:
	  GOTO ID
	  	{}
	;

exprlst:
	  expr
	  	{}
	| expr COMMA exprlst
		{}
	;

asmstm:
	  ASM STRING exprlst
	  	{print_endline "Asm parsed"}
	;


/* XXX: Shift-reduce conflict with rule "number".   */
/*
casechoice:
	  INT COLON block
		{print_endline "Case choice parsed" }
	| CHAR COLON block
		{print_endline "Case choice parsed" }
	| DEFAULT COLON block
		{print_endline "Case choice parsed" }
	;

casechoices:
	  casechoice
	  	{} 
	| casechoice casechoices
		{}
	;

casestm:
	  CASE expr COLON ENDLN casechoices
	  	{print_endline "Case parsed" }
	;
*/

assignment:
	  ASSIGN
	  	{}
	| ASSIGNADD 
		{}
	| ASSIGNSUB
		{}
	| ASSIGNMUL
		{}
	| ASSIGNDIV
		{}
	| ASSIGNMOD
		{}
	| ASSIGNAND
		{}
	| ASSIGNOR
		{}
	| ASSIGNXOR
		{}
	;


assignstm:
	  lexpr SWAP lexpr
	  	{print_endline "Swap parsed" } 
	| lexpr assignment expr 
		{ print_endline "Assignment parsed"}
	;




/* DEFINITONS */


def:
	  fundef
		{}
	| importdecl
		{}
	| usedecl
		{}
	| exportdecl
		{}
	| typedecl
		{}
	| structdecl
		{}
	| constdecl
		{}
	| globaldecl
		{}
	;

defs:
	  def
		{}
	| def ENDLN defs
		{}
	;

constdecl:
	  CONST ID value
	  	{}
	;

globaldecl:
	  GLOBAL ID value
	  	{}
	;

argdeclitem:
	  ID typedec
	  	{}
	| ID COLON value typedec
		{}
	| ID COLON LBRACK RBRACK typedec
		{}
	;

argdecls:
	  argdeclitem
	  	{}
	| argdeclitem COMMA argdecls
		{}
	| /* NOTHING */
		{}
	;


typeparam:
	  LBRACE typedec RBRACE
	  	{}
	| LBRACE typedec typedec RBRACE
		{}
	;


fundef:
	  ID LPAREN argdecls RPAREN COLON block
	  	{print_endline "fundef parsed" }
		
	| ID LPAREN argdecls RPAREN typedec COLON block
	  	{print_endline "fundef w/ ret type parsed" }
		
	| ID typeparam LPAREN argdecls RPAREN COLON block
		{print_endline "fundef w/ type param parsed" }
		
	| ID typeparam LPAREN argdecls RPAREN typedec COLON block
		{print_endline "fundef w/ ret type and param parsed" }
	;


importdecl:
	  IMPORT ID
	  	{}
	;
	
usedecl:
	  USE ID
	  	{}
	;
	
idlst:
	  ID
		{} 
	| ID COMMA idlst
		{}
	;

exportdecl:
	  EXPORT idlst
	  	{}
	;

typedecl:
	  TYPE ID typedec
	  	{}
	;

structitems:
	  vardec ENDLN
	  	{}
	| vardec ENDLN structitems
		{}
	;

structdecl:
	  STRUCT ID COLON ENDLN INDENT structitems DEDENT
	  	{}
	;
%%
(*
arraydec:	
	  LBRACK typedec RBRACK
		{}
	;

ptrdec:
	  PTR typedec
	  	{}
	;

typedec:
	  UINT8
	  	{}
	| INT8
		{ }
	| UINT16
		{ }
	| INT16
		{ }
	| UINT32
		{ }
	| INT32
		{ }
	| UINT64
		{ }
	| INT64
		{ }
	| FLOAT32
		{ }
	| FLOAT64
		{ }
	| NONE 
		{ }
	| STRING
		{ }
	| FUNC
		{ }
	| arraydec
		{ }
	| ptrdec
		{ }
	| ID
		{ }
	;


block:
	INDENT stms DEDENT
		{}
	;

adeflst:
	  value
	  	{}
	| value COMMA adeflst
		{}
	;

arraydefault:
	  LBRACK adeflst RBRACK
	  	{}
	;

vardec:
	/* foo int */
	  ID typedec
	  	{}
	/* foo int <- 10 */
	| ID typedec ASSIGN expr
		{}
	/* foo struct@ (stack var) */
	| ID typedec AT
		{}
	/* foo struct@ <- blah */
	| ID typedec AT ASSIGN expr
		{}
	/* foo [int]@3 (array on stack) */
	| ID typedec AT INT
		{}
	/* foo [int]@3 <- (1, 2, 3) */
	| ID typedec AT INT ASSIGN arraydefault
		{}
	;

eliflst:
	  /* NOTHING */
		{}
	| ELIF expr COLON INDENT stms DEDENT
		{}
	| eliflst eliflst
		{}
	;

ifstm:
	  IF expr COLON block
	  	{}
	| IF expr COLON INDENT stms DEDENT eliflst ELSE block
		{}
	;
	
forstm:
	  FOR stm ENDLN stm ENDLN stm COLON block
	  	{}
	;
	
fromstm:  
	  FROM ID ASSIGN expr TO INT COLON block
	  	{}
	| FROM ID ASSIGN expr TO INT BY INT COLON block
		{}
	;

whilestm:
	  WHILE expr COLON block
	  	{}
	;

assignstm:
	  ID SWAP ID
	  	{} 
	| ID assignment expr 
		{}
	;

trystm:
	  TRY block WITH ID block
	  	{} 
	| TRY block FINALLY block 
		{}
	| TRY block WITH ID block FINALLY block
		{}
	;

gotostm:
	  GOTO ID
	  	{}
	;

asmstm:
	  ASM STRING exprs
	  	{}
	;


casechoice:
	  INT COLON block
		{}
	| CHAR COLON block
		{}
	| DEFAULT COLON block
		{}
	;

casechoices:
	  casechoice
	  	{} 
	| casechoice casechoices
		{}
	;

casestm:
	  CASE expr casechoices
	  	{}
	;


value:
	  number
	  	{}
	| ID
		{}
	| NULL
		{}
	| STRING
		{}
	;

retstm:
	  RET
	  	{}
	| RET expr
		{}
	;

gotolabel:
	  ID HASH
	  	{}
	;

structref:
	  expr PERIOD ID
	  	{}
	;

arrayref:
	  expr PERIOD expr
	  	{}
	;

ptrref:
	  PTR expr
	  	{}
	;
addrref:
	  AND expr
	  	{}
	;

assignment:
	  ASSIGN
	  	{}
	| ASSIGNADD 
		{}
	| ASSIGNSUB
		{}
	| ASSIGNMUL
		{}
	| ASSIGNDIV
		{}
	| ASSIGNMOD
		{}
	| ASSIGNAND
		{}
	| ASSIGNOR
		{}
	| ASSIGNXOR
		{}
	;

raisestm:
	  RAISE ID
	  	{}
	;

stm:
	  exprs
		{}
	| vardec
		{}
	| ifstm
		{}
	| INC ID
		{}
	| DEC ID
		{}
	| forstm
		{}
	| whilestm
		{}
	| assignstm
		{}
	| trystm
		{}
	| gotostm
		{}
	| asmstm
		{}
	| casestm
		{}
	| fromstm
		{}
	| retstm
		{}
	| gotolabel
		{}
	| raisestm
		{}
	;

stms:
	  stm
		{}
	| stm ENDLN stms
		{}
	;

cast:
	  LBRACE typedec RBRACE expr
	  	{}
	;



expr:
	  value
	  	{}
	| cast
		{}
	| funcall
		{}
	| structref 
		{}
	| arrayref
		{}
	| ptrref
		{}
	| arith
		{}
	| logic
		{}
	| binlogic
		{}
	| methodcall
		{}
	| addrref
		{}
	| LPAREN expr RPAREN
		{}
	;

exprs:
	  expr
		{}
	| expr ENDLN expr
		{}
	;

argitem:
	  expr
	  	{}
	| ID COLON expr
		{}
	;
arglst:
	  argitem
	  	{}
	| argitem COMMA argitem
		{} 
	;

funcall:
	  ID LPAREN arglst RPAREN
	  	{}
	| ID RBRACE typedec LBRACE RPAREN arglst LPAREN 
		{}
	;

argdeclitem:
	  ID typedec
	  	{}
	| ID COLON value typedec
		{}
	| ID COLON LBRACK RBRACK typedec
		{}
	;

argdecl:
	  argdeclitem
	  	{}
	| argdeclitem COMMA argdeclitem
		{}
	;

fundef:
	  ID COLON block
		{}  
	| ID LPAREN argdecl RPAREN COLON block
		{}
	| ID LPAREN argdecl RPAREN typedec COLON block
		{}
	| ID typedec COLON block
		{}
	| ID RBRACE typedec LBRACE LPAREN argdecl RPAREN COLON block
		{}
	| ID RBRACE typedec LBRACE LPAREN argdecl RPAREN typedec COLON block
		{}
	;


number:
	  INT
	  	{}
	| FLOAT
		{}
	| CHAR
		{}
	;

arith:
	  expr ADD expr
	  	{}
	| expr SUB expr
		{}
	| expr DIV expr
		{}
	| expr MUL expr
		{}
	| expr MOD expr
		{}
	;
	
logic:
	  expr AND expr
	  	{}
	| expr OR expr
		{}
	| expr XOR expr
		{}
	| NOT expr
		{}
	| expr EQ expr
		{}
	| expr SEQ expr
		{}
	| expr NEQ expr
		{}
	| expr SNEQ expr
		{}
	;
	
/* Remember shl and shr! */
binlogic:
	  expr BAND expr
		{} 
	| expr BOR expr
		{}
	| expr BXOR expr
		{}
	| BNOT expr
		{}
	| expr SHL expr
		{}
	| expr SHR expr
		{}
	;


importdecl:
	  IMPORT ID
	  	{}
	;
	
usedecl:
	  USE ID
	  	{}
	;
	
exportlst:
	  ID
		{} 
	| ID COMMA exportlst
		{}
	;

exportdecl:
	  EXPORT exportlst
	  	{}
	;

typedecl:
	  TYPE ID typedec
	  	{}
	;

structitems:
	  vardec ENDLN
	  	{}
	| vardec ENDLN structitems
		{}
	;

structdecl:
	  STRUCT ID COLON ENDLN INDENT structitems DEDENT
	  	{}
	;
	

classmember:
	  vardec
	  	{}
	| methoddec
		{}
	;

classmembers:
	  classmember
	  	{}
	| classmember ENDLN classmembers
		{}
	;

classdec:
	  CLASS ID COLON ENDLN INDENT classmembers DEDENT
	  	{}
	| CLASS ID LPAREN ID RPAREN COLON ENDLN INDENT classmembers DEDENT
		{}
	;

methoddec:
	  METHOD fundef
	  	{}
	;
	
methodcall:
	  ID METHODCALL funcall
	  	{}
	;


dec:
	  classdec
	  	{}
	| fundef
		{}
	| importdecl
		{}
	| usedecl
		{}
	| exportdecl
		{}
	| typedecl
		{}
	| structdecl
		{}
	;
decs:
	  dec
		{}
	| dec ENDLN decs
		{}
	;
*)
