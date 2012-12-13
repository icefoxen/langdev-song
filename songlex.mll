{
(* Hmm...  okay.  Lexer for Song.  As usual, strings are bitchy; other things
   are generally simple.

   Simon Heath
   8/03/2004
*)


open ErrorReport;;
open Songparse;;
open Syntree;;


exception Lexer_error


let gs = Lexing.lexeme;;
let gi x = int_of_string (gs x);;
let gf x = float_of_string (gs x);;


let inComment = ref 0;;


(* Advances the position of the error-checking vars. *)
let adv lb =
  let c = (gs lb) in
(*  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb); *)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;


(* Parses a binary string and returns an int *)
let b2i str =
   let res = ref 0.0
   and p = ref 0.0 in
   for i = String.length( str ) - 1 downto 0 do
      if (String.get str i) = '1' then
         res := !res +. (2.0 ** !p)
      else ();
      p := !p +. 1.
   done;
   int_of_float !res;;

let s2c str =
  if str.[1] = '\\' then
    match str.[2] with
	'0' -> char_of_int (int_of_string (String.sub str 2 ((String.length str) - 3)))
      | 'n' -> '\n'
      | 'b' -> '\b'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '\'' -> '\''
      | '\\' -> '\\'
      | _ -> (error ("Invalid char escape: " ^ str )); (raise Lexer_error)
  else
    str.[1]
;;



let string_of_char c =
  String.make 1 c;;  

(* XXX: Fix!  Is there a better way than going through and examining each
   character? 
   Ah, got it.  Build a new string or array rather than trying to modify it 
   in-place.  It'll be at most as long as the original, and you can chop it
   down later.

   Not perfect; notably, it only looks one character ahead and doesn't resolve
   binary number-literals.
   Who'da thunk such a straightforward problem would be so bitchy?
   Maybe a string-search algorithm would be handy...
   Baka.  Ocaml HAS string-searching and replace algorithms.
   ...they're utterly freakin' whacked, but it has them.  <_<
   XXX: Fix.  Why the hell is \r sticking \008 in random places?
   And why the hell are random \'s appearing --only sometimes though?
*)

let parseStr x = x;;
(*
let parseStr x = 
      print_endline "BAR:";
      print_endline x;
      print_endline "\nUNBAR";
      flush stdout;
   let r1 s = Str.global_replace (Str.regexp "\\r") "\r" s; s
   and r2 s = Str.global_replace (Str.regexp "\\\\n") "\n" s
   and r3 s = Str.global_replace (Str.regexp "\\b") "\b" s; s
   and r4 s = Str.global_replace (Str.regexp "\\t") "\t" s
   and r5 s = Str.global_replace (Str.regexp (String.make 4 '\\')) "\\\\" s
   and r6 s = Str.global_replace (Str.regexp "\\[0-9]+") "\\1" s
   in
      let f = (r1 (r2 (r3 (r4 (r5 (r6 x)))))) in

      print_endline "FOO:";
      print_endline (String.escaped f);
      print_endline f;
      print_endline "\nUNFOO";
      flush stdout;
      f
;;
*)

(*
let parseStr x =
   let oldstrlen = (String.length x) - 1 in
   let newstr = String.make (oldstrlen + 1) '\000' 
   and newstrlen = ref 0 
   and strpos = ref 0 in
      while x.[!strpos] <> '"' do
         if x.[!strpos] = '\\' then
	   let chr = match x.[!strpos + 1] with
               | 'n' -> '\n'
               | 'b' -> '\b'
               | 'r' -> '\r'
               | 't' -> '\t'
               | '\'' -> '\''
               | '\\' -> '\\'
               | _ -> (error ("Invalid char escape: " ^ x )); 
	              (raise Lexer_error)
	      in
	      incr newstrlen;
	      newstr.[!newstrlen] <- chr;
	      strpos := !strpos + 2;
	 else (
	     newstr.[!newstrlen] <- x.[!strpos];
	     incr newstrlen;
	     incr strpos;
	 )
      done;
   String.sub newstr 0 !newstrlen
;;
*)

}

let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* 

let inum = '-'?['0'-'9']+ | "0x"['0'-'9''a'-'f''A'-'F']+ | "0o"['0'-'7']+ 
let bnum = "0b" ['0'-'1']+

let fnum = '-'?['0'-'9']+ '.' ['0'-'9']*

let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")



(* TOKENS:
ID INT FLOAT CHAR STRING 
( ) [ ] { } . , @ ; ;; end ... :
<- <-+ <-- <-* <-/ <-% <-& <-|
and or not xor & | ! ^ ~
+ - * / % inc dec = == /= /== <= >= < >
module import use global const protocol class type struct export
if elif else while do for from to by try with finally # goto version def sizeof
new alloc static free asm
continue break ret

int8  uint8  byte ubyte
int16 uint16 short ushort
int32 uint32 int  float32 float
int64 uint64 long float64 double
char str bool NONE

*)

rule start = parse
   (* Erm, I sorta changed the type parsing mid-stride, so Songparse and
      Syntree define some types with the same names... hehehe...  ^_^;; *)
   inum			{ adv lexbuf; Songparse.INT( gi lexbuf ) }
 | bnum			{ adv lexbuf; Songparse.INT( b2i (gs lexbuf) ) }
 | fnum			{ adv lexbuf; Songparse.FLOAT( gf lexbuf ) }
 | chr                  { adv lexbuf; Songparse.CHAR( s2c (gs lexbuf) ) }
 | '\n'                 { nl (); start lexbuf }
 | '\r'                 { start lexbuf }
 | ' '                  { adv lexbuf; start lexbuf }
 | '\t'                 { adv lexbuf; start lexbuf }
 | "//"			{ adv lexbuf; adv lexbuf; lcomment lexbuf }
 | "{-"			{ adv lexbuf; inComment := !inComment + 1; bcomment lexbuf; }
 | "$"			{ adv lexbuf; ARREF }
 | "end"		{ adv lexbuf; END }
 | ";;"			{ adv lexbuf; END }
 | "::"			{ adv lexbuf; PKGREF }
 | '"'			{ adv lexbuf; grabstr lexbuf }
 | "("			{ adv lexbuf; LPAREN }
 | ")"			{ adv lexbuf; RPAREN }
 | "["			{ adv lexbuf; LBRACK }
 | "]"			{ adv lexbuf; RBRACK }
 | "{"			{ adv lexbuf; LBRACE }
 | "}"			{ adv lexbuf; RBRACE }
 | "{!"			{ adv lexbuf; ARRAYSTART }
 | "!}"			{ adv lexbuf; ARRAYEND }
 | "."			{ adv lexbuf; PERIOD }
 | ","			{ adv lexbuf; COMMA }
 | "@"			{ adv lexbuf; AT }
 | ";"			{ adv lexbuf; ENDSTM }
 | ":"			{ adv lexbuf; COLON }
 | "&"			{ adv lexbuf; BAND }
 | "|"			{ adv lexbuf; BOR }
 | "^"			{ adv lexbuf; BXOR }
 | "~"			{ adv lexbuf; BCOMP }
 | "!"			{ adv lexbuf; BNOT }
 | "+"			{ adv lexbuf; ADD }
 | "-"			{ adv lexbuf; SUB }
 | "*"			{ adv lexbuf; MUL }
 | "/"			{ adv lexbuf; DIV }
 | "%"			{ adv lexbuf; MOD }
 | "#"			{ adv lexbuf; LABEL }
 | "<-"			{ adv lexbuf; ASSIGN }
 | "<-+"		{ adv lexbuf; ASSIGNADD }
 | "<--"		{ adv lexbuf; ASSIGNSUB }
 | "<-*"		{ adv lexbuf; ASSIGNMUL }
 | "<-/"		{ adv lexbuf; ASSIGNDIV }
 | "<-%"		{ adv lexbuf; ASSIGNMOD }
 | "<-&"		{ adv lexbuf; ASSIGNAND }
 | "<-|"		{ adv lexbuf; ASSIGNOR }
 | "and"		{ adv lexbuf; AND }
 | "or"			{ adv lexbuf; OR }
 | "not"		{ adv lexbuf; NOT }
 | "xor"		{ adv lexbuf; XOR }
 | "<<"                 { adv lexbuf; SHL }
 | ">>"                 { adv lexbuf; SHR }
 | "inc"		{ adv lexbuf; INC }
 | "dec"		{ adv lexbuf; DEC }
 | "=="			{ adv lexbuf; SEQ }
 | "="			{ adv lexbuf; EQ }
 | "/=="		{ adv lexbuf; SNEQ }
 | "/="			{ adv lexbuf; NEQ }
 | ">="			{ adv lexbuf; GEQ }
 | "<="			{ adv lexbuf; LEQ }
 | ">"			{ adv lexbuf; GT }
 | "<"			{ adv lexbuf; LT }
 | "module"		{ adv lexbuf; MODULE }
 | "import"		{ adv lexbuf; IMPORT }
 | "use"		{ adv lexbuf; USE }
 | "export"		{ adv lexbuf; EXPORT }
 | "global"		{ adv lexbuf; GLOBAL }
 | "const"		{ adv lexbuf; CONST }
 | "protocol"		(* { adv lexbuf; PROTOCOL } *)
 				{ fatalError "Protocols not implemented!" }
 | "class"		(* { adv lexbuf; CLASS } *)
 				{ fatalError "Classes not implemented!" }
 | "type"		{ adv lexbuf; TYPE }
 | "struct"		{ adv lexbuf; STRUCT }
 | "if"			{ adv lexbuf; IF }
 | "then"		{ adv lexbuf; THEN }
 | "elif"		{ adv lexbuf; ELIF }
 | "else"		{ adv lexbuf; ELSE }
 | "for"		{ adv lexbuf; FOR }
 | "do"			{ adv lexbuf; DO }
 | "from"		{ adv lexbuf; FROM }
 | "to"			{ adv lexbuf; TO }
 | "by"			{ adv lexbuf; BY }
 | "while"		{ adv lexbuf; WHILE }
 | "try"		{ adv lexbuf; TRY }
 | "with"		{ adv lexbuf; WITH }
 | "finally"		{ adv lexbuf; FINALLY }
 | "goto"		{ adv lexbuf; GOTO }
 | "version"		{ adv lexbuf; VERSION }
 | "def"		{ adv lexbuf; DEF }
 | "sizeof"		{ adv lexbuf; SIZEOF }
 | "new"		{ adv lexbuf; Songparse.NEW }
 (* Manual memory management is obselete.
 | "alloc"		{ adv lexbuf; Songparse.ALLOC } *)
 (* | "static"		{ adv lexbuf; Songparse.STATIC } *)
 (* | "free"		{ adv lexbuf; FREE } *)
 | "asm"		{ adv lexbuf; ASM }
 | "continue"		{ adv lexbuf; CONTINUE }
 | "break"		{ adv lexbuf; BREAK }
 | "ret"		{ adv lexbuf; RET }

 | "int8"		{ adv lexbuf; TYPENAME( INTSIZE( 1 ) ) }
 | "uint8"		{ adv lexbuf; TYPENAME( UINTSIZE( 1 ) ) }
 | "byte"		{ adv lexbuf; TYPENAME( BYTE ) }
 | "int16"		{ adv lexbuf; TYPENAME( INTSIZE( 2 ) ) }
 | "uint16"		{ adv lexbuf; TYPENAME( UINTSIZE( 2 ) ) }
 | "short"		{ adv lexbuf; TYPENAME( SHORT ) }
 | "ushort"		{ adv lexbuf; TYPENAME( USHORT ) }
 | "int32"		{ adv lexbuf; TYPENAME( INTSIZE( 4 ) ) }
 | "uint32"		{ adv lexbuf; TYPENAME( UINTSIZE( 4 ) ) }
 | "int"		{ adv lexbuf; TYPENAME( INT ) }
 | "uint"		{ adv lexbuf; TYPENAME( UINT ) }
 | "int64"		{ adv lexbuf; TYPENAME( INTSIZE( 8 ) ) }
 | "uint64"		{ adv lexbuf; TYPENAME( UINTSIZE( 8 ) ) }
 | "long"		{ adv lexbuf; TYPENAME( LONG ) }
 | "ulong"		{ adv lexbuf; TYPENAME( ULONG ) }
 | "float32"		{ adv lexbuf; TYPENAME( FLOATSIZE( 4 ) ) }
 | "float"		{ adv lexbuf; TYPENAME( Syntree.FLOAT ) }
 | "float64"		{ adv lexbuf; TYPENAME( FLOATSIZE( 8 ) ) }
 | "double"		{ adv lexbuf; TYPENAME( Syntree.DOUBLE ) }
 | "char"		{ adv lexbuf; TYPENAME( Syntree.CHAR ) }
 | "str"		{ adv lexbuf; TYPENAME( STR  ) }
 | "bool"		{ adv lexbuf; TYPENAME( BOOL ) }
 | "NONE"		{ adv lexbuf; TYPENAME( NONE ) }
 (* Erm, this one gets a bit convoluted, mainly due to gaps in the spec
    concerning the definition of function vars.
    All right, all right, it's a kludge, and will have to be fixed when
    I fix the parser to recognize function types with vars and return types
    attached.
  *)
 | "func"		{ adv lexbuf; 
                          TYPENAME( 
                           FUNC( [("args", Basictype( NONE ), Nullstm, true)],
			         [Ptrtype( Basictype( NONE ) )] ) ) 
			}


 | id			{ adv lexbuf; ID( gs lexbuf ) }
 | eof			{ EOF }
 | _			{ adv lexbuf; 
                          error ("Invalid token: " ^ (gs lexbuf)); 
			  raise Lexer_error; }

and lcomment = parse
   '\n'			{ nl (); start lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }


and bcomment = parse
   "{-"			{ adv lexbuf; inComment := !inComment + 1; 
                          bcomment lexbuf }
 | "-}"			{ adv lexbuf; inComment := !inComment - 1; 
                          if !inComment <= 0 then start lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 

and grabstr = parse
   '"'			{ adv lexbuf; start lexbuf }
 | ("\\\""|[^'"'])*'"'	{ 
   			adv lexbuf; 
			Songparse.STR( parseStr  (gs lexbuf) ) }
			       (*(String.sub (gs lexbuf) 
			          0 
				  ((String.length 
				      (gs lexbuf)) - 2) )) } 
				 (Str.first_chars (gs lexbuf) 
				    ((String.length (gs lexbuf)) - 1))) } *)



{
}
