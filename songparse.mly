%{
(* songparse.mly
   Parser for song, obviously.

   Simon Heath
   06/08/2004
*)

open Syntree;;

exception TypeError;;


let append x y = [x] @ y;;

let addimport x y   = x.importlst  <- append y x.importlst;;
let adduse x y      = x.uselst     <- append y x.uselst;;
let addexport x y   = x.exportlst  <- append y x.exportlst;;
let addconst x y    = x.constlst   <- append y x.constlst;;
let addglobal x y   = x.globallst  <- append y x.globallst;;
let addtype x y     = x.typelst    <- append y x.typelst;;
let addfun x y      = x.funlst     <- append y x.funlst;;
let addversion x y  = x.versionlst <- append y x.versionlst;;

let currentmodule = {
   name       = "";
   importlst  = [];
   uselst     = [];
   exportlst  = [];
   constlst   = [];
   globallst  = [];
   typelst    = [];
   funlst     = [];
   versionlst = [];
};;

let reset () =
   currentmodule.name 		<- "";
   currentmodule.importlst	<- [];
   currentmodule.uselst		<- [];
   currentmodule.exportlst	<- [];
   currentmodule.constlst	<- [];
   currentmodule.globallst	<- [];
   currentmodule.typelst	<- [];
   currentmodule.funlst		<- [];
   currentmodule.versionlst	<- [];
;;

%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STR
%token <Syntree.typeident> TYPENAME
%token <string> ID

%token END PKGREF ARRAYSTART ARRAYEND ARREF
%token AND OR NOT XOR SHL SHR INC DEC SEQ EQ SNEQ NEQ GEQ LEQ GT LT
%token ASSIGN ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV ASSIGNMOD
%token ASSIGNAND ASSIGNOR
%token MODULE IMPORT USE EXPORT GLOBAL CONST PROTOCOL CLASS TYPE STRUCT
%token IF THEN ELIF ELSE FOR DO FROM TO BY WHILE TRY WITH FINALLY
%token GOTO VERSION DEF SIZEOF NEW ALLOC STATIC FREE ASM
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PERIOD COMMA AT ENDSTM COLON BAND BOR BXOR BCOMP BNOT
%token ADD SUB MUL DIV MOD LABEL
%token CONTINUE BREAK RET
%token EOF


%type <Syntree.moduleStruct> main
%start main

%%

main: 
	  moduledecl decllst EOF
	  	{ currentmodule }
	| decllst EOF
		{ currentmodule }
	;

decllst:
	  decl
	  	{}
	| decl decllst
		{}
	;

decl:
	  importdecl
	  	{}
	| usedecl
		{}
	| globaldecl
		{}
	| constdecl
		{}
	| protocoldecl
		{}
	| classdecl
		{}
	| fundecl
		{ addfun currentmodule $1 }
	| typedecl
		{}
	| versiondecl
		{}
	| exportdecl
		{}
	| structdecl
		{}
	;

moduledecl:
	  MODULE ID ENDSTM
	  	{ currentmodule.name <- $2 }
	;

importdecl:
	  IMPORT ID ENDSTM
	  	{ addimport currentmodule $2 }
	;

usedecl:
	  USE ID ENDSTM
	  	{ adduse currentmodule $2 }
	;

constdecl:
	  CONST ID typename ASSIGN value ENDSTM
	  	{ addconst currentmodule (makevar $2 $3 $5) }
	;

globaldecl:
	  GLOBAL ID typename ENDSTM
	  	{ addglobal currentmodule (makevar $2 $3 Nullstm) }
	| GLOBAL ID typename ASSIGN value ENDSTM
	  	{ addglobal currentmodule (makevar $2 $3 $5) }
	;
versiondecl:
	  DEF ID ENDSTM
	  	{ addversion currentmodule $2 }
	;

idlst:
	  ID
	  	{ [$1] }
	| ID COMMA idlst
		{ append $1 $3 }
	;

typenamelst:
	  typename
		{ [$1] }
	| typename COMMA typenamelst
		{ append $1 $3 }
	;

exportdecl:
	  EXPORT idlst ENDSTM
	  	{ currentmodule.exportlst <- currentmodule.exportlst @ $2 }
	;

protocoldecl:
	  PROTOCOL ID EQ funheaderlst END
	  	{}
	| PROTOCOL ID LPAREN ID RPAREN EQ funheaderlst END
		{}
	| PROTOCOL typenamelst ID LPAREN funheaderlst END
		{}
	| PROTOCOL typenamelst ID LPAREN ID RPAREN EQ funheaderlst END
		{}
	;

pkgref:
	  ID
	  	{ ("", $1) }
	| ID PKGREF ID
		{ ($1, $3) }
	;

classdecl:
	  CLASS pkgref COLON ID EQ fundecllst END
	  	{}
	;

/* XXX: No compound types?  int List, for instance... 
   Well, no typeclasses yet, so it doesn't really matter.
   Ummm... yeah, it does actually.  Hrm.  This bites.
   How can types be qualified to another type?  foo{bar}?  That might
   very well work.
   ...But then what about functions?  Snarl.  This is one of the MAJOR
   improvements over C.  Grar.
   Hmmmmm.  Basically, compound types/typenames can occur in four places 
   (I think):
   * Var declerations (in functions or types)
   * Function declerations
   * Function parameters
   * Coercions
   Soooo....  foo{x,y} a x, b y = ...
   Or foo a @x, b @y = ...
   But then what about typechecking?  How does that change?
   ...maybe I should just make a language with dynamic type, heh.
   Or even untyped, a la BCPL or Forth.  <_<
   Grrr.  I don't like this complexity!  How does Haskell do it?  Type 
   inferance.  Pascal, Modula?  They don't have this feature.  C++, Eiffel?
   More complexity.
   Maybe I should read up on type-inferance algorithms.
   XXX: I can parse this, one way or another...  But verifying it may be a
   pain.  OCaml does this by having one internal type and tag bits...
   XXX:  Oop ack.  Bah.  Screw it.  No varient types in this version.
   Get it working first, then worry about fancy bips and boodles.
   Ask again next release.  
*/
typename:
	  TYPENAME
	  	{ Basictype( $1 ) }
	| BXOR typename
		{ Ptrtype( $2 ) }
	| LBRACK typename RBRACK
	  	{ Arraytype( $2 ) }
	/*
	| LBRACE idlst RBRACE pkgref
		{ VarientType( $4, $2 ) }
	*/
	| pkgref
		{ let a, b = $1 in Customtype( a, b ) }
	;

value:
	  INT
		{ Intstm( $1 ) }
	| FLOAT
		{ Floatstm( $1 ) }
	| CHAR
		{ Charstm( $1 ) }
	| STR
		{ Strstm( $1 ) }
	| arrayliteral
		{ Arraylitstm( $1 ) }
	| pkgref
		{ Valstm( $1 ) }
	;

valuelst:
	  value
		{ [$1] }
	| value COMMA valuelst
		{ append $1 $3 }
	;

arrayliteral:
	  ARRAYSTART valuelst ARRAYEND
	  	{ $2 }
	;

paramlst:
	  ID typename
		{ [($1, $2, Nullstm, (if $1 = "args" then true else false))] }
	| ID typename COMMA paramlst
		{ append ($1, $2, Nullstm, false) $4 }
	;

funheader:
	  ID
	  	{ ($1, [], [Nulltype]) }
	| ID paramlst
		{ ($1, $2, [Nulltype]) }
	| ID COLON typenamelst
		{ ($1, [], $3) }
	| ID paramlst COLON typenamelst
		{ ($1, $2, $4) }
	;

funheaderlst:
	  funheader END
	 	{ [$1] }
	| funheader END funheaderlst
		{ append $1 $3 }
	;
	

fundecl:
	  funheader EQ stmlst END
	  	{  (makefun $1 $3) }
	| funheader EQ  END
	  	{ (makefun $1 []) }
	;

fundecllst:
	  fundecl
	  	{ [$1] }
	| fundecl fundecllst
		{ append $1 $2 }
	;

typedecl: 
	  TYPE ID EQ typename ENDSTM
	  	{ addtype currentmodule (Typedecl( $2, $4 )) }
	;

/* ...Can structs be initialized to function values?
   Are there constructor/init functions?
   Something to figure out later.
   Same with globals being initialized...
   XXX: No, to both.
*/
structmember:
	  ID typename ENDSTM
	  	{ makevar $1 $2 Nullstm }
	| ID typename ASSIGN value ENDSTM
		{ makevar $1 $2 $4 }
	;

structmemberlst:
	  structmember
	 	{ [$1] }
	| structmember structmemberlst
		{ append $1 $2 }
	;

structdecl: 
	  STRUCT ID EQ structmemberlst END
	  	{ addtype currentmodule (Structdecl( $2, $4 )) }
	;

stm:
	  ifstm
		{ $1 } 
	| whilestm
		{ $1 } 
	| forstm
		{ $1 } 
	| fromstm
		{ $1 } 
	| trystm
		{ $1 } 
	| gotolabel
		{ $1 } 
	| gotostm
		{ $1 } 
	| versionstm
		{ $1 } 
	| varstm
		{ $1 } 
	| fundecl
		{ Fundeclstm( $1 ) } 
	| asmstm
		{ $1 } 
	/*
	| freestm
		{ $1 } 
	*/
	| assignstm
		{ $1 } 
	| retstm
		{ $1 } 
	| breakstm
		{ $1 } 
	| continuestm
		{ $1 }
	| expr ENDSTM
		{ $1 }
	;

stmlst:
	  stm
	  	{ [$1] }
	| stm stmlst
		{ append $1 $2 }
	;

ifstm:
	  IF expr THEN stmlst END
	  	{ Ifstm( $2, $4, [], [] ) }
	| IF expr THEN stmlst eliflst END
	  	{ Ifstm( $2, $4, $5, [] ) }
	| IF expr THEN stmlst ELSE stmlst END
	  	{ Ifstm( $2, $4, [], $6 ) }
	| IF expr THEN stmlst eliflst ELSE stmlst END
	  	{ Ifstm( $2, $4, $5, $7 ) }
	;

elifstm:
	  ELIF expr THEN stmlst
	  	{ ($2, $4) }
	;

eliflst:
	  elifstm
	  	{ [$1] }
	| elifstm eliflst
		{ append $1 $2 }
	;

whilestm:
	  WHILE expr DO stmlst END
	  	{ Whilestm( $2, $4 ) }
	;

forstm:
	  FOR varstm expr ENDSTM expr DO stmlst END
		{ Forstm( $2, $3, $5, $7 ) }
	;

fromstm:
	  FROM ID ASSIGN expr TO expr DO stmlst END
	  	{ Fromstm( 
		    (makevar $2 
		             (Basictype( Syntree.INT )) $4),
		    $6, Intstm( 1 ), $8 ) }
	| FROM ID ASSIGN expr TO expr BY expr DO stmlst END
	  	{ Fromstm( 
		    (makevar $2 
		             (Basictype( Syntree.INT )) $4),
		    $6, $8, $10 ) }
	;

trystm:
	  TRY stmlst WITH ID DO stmlst END
	  	{ Trystm( $2, $4, $6, [] ) }
	| TRY stmlst WITH ID DO stmlst FINALLY stmlst END
	  	{ Trystm( $2, $4, $6, $8 ) }
	| TRY stmlst FINALLY stmlst END
	  	{ Trystm( $2, "", [], $4 ) }
	;

gotolabel:
	  LABEL ID ENDSTM
	  	{ Gotolabel( $2 ) }
	;

gotostm:
	  GOTO ID ENDSTM
	  	{ Gotostm( $2 ) }
	;

versionstm:
	  VERSION ID DO stmlst END
	  	{ Versionstm( $2, $4 ) }
	;

varnamelst:
	  ID typename
		{ [makevar $1 $2 Nullstm] }
	| ID typename COMMA varnamelst
		{ append (makevar $1 $2 Nullstm) $4 }
	;

varstm:
	  varnamelst
	  	{ Vardeclstm( $1, [Nullstm] ) }
	| varnamelst ASSIGN commaexprlst ENDSTM
		{ Vardeclstm( $1, $3 ) }
	;

/*
freestm:
	  FREE pkgref ENDSTM
	  	{ Freestm( $2 ) }
	;
*/

asmstm:
	  ASM STR ENDSTM
	  	{ Asmstm( $2 ) }
	;

lval:
	  BXOR expr
	  	{ Ptrstm( $2 ) }
	| arrayexpr
		{ $1 }
	| structexpr
		{ $1 }
	| pkgref
		{ Valstm( $1 ) }
	;

lvallst:
	  lval
	  	{ [$1] }
	| lval COMMA lvallst
		{ append $1 $3 }
	;

assignstm:
	  lvallst ASSIGN commaexprlst ENDSTM
	  	{ Assignstm( $1, $3 ) }
	| lval ASSIGNADD expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "ADD", [$1; $3] )] ) }
	| lval ASSIGNSUB expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "SUB", [$1; $3] )] ) }
	| lval ASSIGNMUL expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "MUL", [$1; $3] )] ) }
	| lval ASSIGNDIV expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "DIV", [$1; $3] )] ) }
	| lval ASSIGNMOD expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "MOD", [$1; $3] )] ) }
	| lval ASSIGNAND expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "AND", [$1; $3] )] ) }
	| lval ASSIGNOR expr ENDSTM
	  	{ Assignstm( [$1], [Primop( "OR", [$1; $3] )] ) }
	;

commaexprlst:
	  expr
	  	{ [$1] }
	| expr COMMA commaexprlst
		{ append $1 $3 }
	;

retstm:
	| RET commaexprlst ENDSTM
		{ Retstm( $2 ) }
	;

breakstm:
	  BREAK ENDSTM
	  	{ Breakstm }
	;

continuestm:
	  CONTINUE ENDSTM
	  	{ Continuestm }
	;

spaceexprlst:
	  expr
	  	{ [$1] }
	| expr spaceexprlst
		{ append $1 $2 }
	;

expr:
	  funcall
	  	{ $1 }
	| coerceexpr
	  	{ $1 }
	| sizeofexpr
	  	{ $1 }
	| arrayexpr
	  	{ $1 }
	| structexpr
	  	{ $1 }
	| value
	  	{ $1 }
	| allocexpr
	  	{ $1 }
	| prefixexpr
	  	{ $1 }
	;
	

funcall:
	  /*LPAREN primop expr RPAREN
	  	{ Primop( $2, $3, Nullstm ) }
	| LPAREN primop expr expr RPAREN
	  	{ Primop( $2, $3, $4 ) } */
	  LPAREN primop spaceexprlst RPAREN
	  	{ Primop( $2, $3 ) }
	| LPAREN pkgref RPAREN
	  	{ Funcall( $2, [] ) }
	| LPAREN pkgref spaceexprlst RPAREN
		{ Funcall( $2, $3 ) }
	;

primop:
	  ADD
	  	{ "ADD" }
	| SUB
		{ "SUB" }
	| MUL
		{ "MUL" }
	| DIV
		{ "DIV" }
	| MOD
		{ "MOD" }
	| AND
		{ "AND" }
	| OR
		{ "OR" }
	| NOT
		{ "NOT" }
	| XOR
		{ "XOR" }
	| BAND
		{ "BAND" }
	| BOR
		{ "BOR" }
	| BXOR
		{ "BXOR" }
	| BNOT
		{ "BNOT" }
	| BCOMP
		{ "BCOMP" }
	| EQ
		{ "EQ" }
	| NEQ
		{ "NEQ" }
	| SEQ
		{ "SEQ" }
	| SNEQ
		{ "SNEQ" }
	| GT
		{ "GT" }
	| LT
		{ "LT" }
	| GEQ
		{ "GEQ" }
	| LEQ
		{ "LEQ" }
	| INC
		{ "INC" }
	| DEC
		{ "DEC" }
	;

/* Should this be typename "<:" expr instead, a la M3?  Cleaner, harder...
   Could free { ... } for array declerations and so on.
*/
coerceexpr:
	  LBRACE typename RBRACE expr
	  	{ Coercestm( $2, $4 ) }
	;

sizeofexpr:
	  LPAREN SIZEOF typename RPAREN
	  	{ Sizeofstm( $3 ) }
	;

prefixexpr:
	  BXOR expr
	  	{ Ptrstm( $2 ) }
	/* No direct address operator, please. */
	/*| BAND expr
		{ Addrstm( $2 ) } */
	;


arrayseries:
	  expr
	  	{ $1 }
	| expr COMMA arrayseries
		{ Arraystm( $1, $3 ) }
	;

arrayexpr:
	  LBRACK expr arrayseries RBRACK
	  	{ Arraystm( $2, $3 ) }
	;

/* This unfortunately causes a shift-reduce, but it's the simplest way... */
periodidlst:
	  ID
	  	{ [$1] }
	|  periodidlst PERIOD ID
		{ append $3 $1 }
	;

/* Not exactly ideal, buuuut...  Best we can do starting with a plain expr
   instead of worrying about left-recursion.  I'm sure there's a way around
   it, but...
*/
structexpr:
	  PERIOD expr PERIOD periodidlst
	  	{ Structstm( $2, $4 ) }
	; 


allocexpr:
	  NEW typename LPAREN RPAREN
	  	{ Allocstm( NEW, $2, Intstm( 1 ) ) }
	| NEW typename LPAREN expr RPAREN
	  	{ Allocstm( NEW, $2, $4 ) }
	/* Memory management is obselete. */
	/*| ALLOC typename LPAREN RPAREN
	  	{ Allocstm( ALLOC, $2, Intstm( 1 ) ) }
	| ALLOC typename LPAREN expr RPAREN
	  	{ Allocstm( ALLOC, $2, $4 ) } */
	/*| STATIC typename LPAREN RPAREN
	  	{ Allocstm( STATIC, $2, Intstm( 1 ) ) }
	| STATIC typename LPAREN expr RPAREN
	  	{ Allocstm( STATIC, $2, $4 ) } */
	;

