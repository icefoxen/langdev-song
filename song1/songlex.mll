(* songlex.mll
   A lexer for Song.  Ocamllex.  Good stuff.

   XXX: Write parsechr and parsestr!

   Simon Heath
   18/2/2004
*)

{

open Songparse
open ErrorReport
exception Eof
exception Lexer_error

let inComment = ref 0;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  let c = (gs lb) in
  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
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
   Int64.of_float !res;;

let parseChr str =
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
    str.[0]
;;

let string_of_char c =
  String.make 1 c;;  

(* Massively inefficient, but hopefully it'll work.  *)
let parseStr str =
  let s = ref "" in
    for x = 0 to (String.length str) - 1 do
      if str.[x] = '\\' then
	s := !s ^ 
	(string_of_char 
	   (parseChr 
	      (String.sub str x (String.length str - x))))
      else
	s := !s ^ (string_of_char str.[x])
    done;
    !s
;;


}


let id = 
  ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*

let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")


rule token = parse
   ' '                  { adv lexbuf; token lexbuf } (* Skip blanks *)
 | '\t'			{ print_endline "ERROR: Tabs are illegal!"; raise Lexer_error }
 | inum			{ adv lexbuf; INT( Int64.of_string (gs lexbuf) ) }
 | fnum			{ adv lexbuf; FLOAT( float_of_string (gs lexbuf) ) }
 | bnum			{ adv lexbuf; INT( b2i (gs lexbuf) ) }
 | chr                  { adv lexbuf; CHAR( parseChr (gs lexbuf) ) }
 | "_INDENT"            { adv lexbuf; print_endline "Indented"; INDENT }
 | "_DEDENT"            { adv lexbuf; print_endline "Dedented"; DEDENT }
 | "\\\n"               { adv lexbuf; token lexbuf } (* Continue line *)
 | '\n'                 { nl ();  print_endline "ENDLN snaffled"; ENDLN }
 | '\r'                 { token lexbuf }
 | ";"                  { adv lexbuf; print_endline "ENDLN snaffled"; ENDLN }
 | "<-"                 { adv lexbuf; ASSIGN }
 | "<->"                { adv lexbuf; SWAP }
 | "<-+"                { adv lexbuf; ASSIGNADD }
 | "<--"                { adv lexbuf; ASSIGNSUB }
 | "<-*"                { adv lexbuf; ASSIGNMUL }
 | "<-/"                { adv lexbuf; ASSIGNDIV }
 | "<-%"                { adv lexbuf; ASSIGNMOD }
 | "<-&"                { adv lexbuf; ASSIGNAND }
 | "<-|"                { adv lexbuf; ASSIGNOR }
 | "<-~"                { adv lexbuf; ASSIGNXOR }
 | ">="                 { adv lexbuf; GTE }
 | "<="                 { adv lexbuf; LTE }
 | ">"                  { adv lexbuf; GT }
 | "<"                  { adv lexbuf; LT }
 | "=="                 { adv lexbuf; SEQ }
 | "/=="                { adv lexbuf; SNEQ }
 | "/="                 { adv lexbuf; NEQ }
 | "="                  { adv lexbuf; EQ }
 | "("                  { adv lexbuf; LPAREN }
 | ")"                  { adv lexbuf; RPAREN }
 | "::"			{ adv lexbuf; METHODCALL }
 | ":"                  { adv lexbuf; COLON }
 | "["                  { adv lexbuf; LBRACK }
 | "]"                  { adv lexbuf; RBRACK }
 | "@"                  { adv lexbuf; AT }
 | "{"                  { adv lexbuf; LBRACE }
 | "}"                  { adv lexbuf; RBRACE }
 | "."                  { adv lexbuf; PERIOD }
 | ","                  { adv lexbuf; COMMA }
 | "#"                  { adv lexbuf; HASH }
 | "and"                { adv lexbuf; AND }
 | "or"                 { adv lexbuf; OR }
 | "not"                { adv lexbuf; NOT }
 | "xor"                { adv lexbuf; XOR }
 | "inc"                { adv lexbuf; INC }
 | "dec"                { adv lexbuf; DEC }
 | "&"                  { adv lexbuf; BAND }
 | "|"                  { adv lexbuf; BOR }
 | "!"                  { adv lexbuf; BNOT }
 | "~"                  { adv lexbuf; BXOR }
 | "<<"			{ adv lexbuf; SHL }
 | ">>"			{ adv lexbuf; SHR }
 | "+"                  { adv lexbuf; ADD }
 | "-"                  { adv lexbuf; SUB }
 | "*"                  { adv lexbuf; MUL }
 | "/"                  { adv lexbuf; DIV }
 | "\""                 { adv lexbuf; grabstr lexbuf }
 | "%"                  { adv lexbuf; MOD }
 | "sizeof"		{ adv lexbuf; SIZEOF }
 | "raise"              { adv lexbuf; RAISE }
 | "with"               { adv lexbuf; WITH }
 | "finally"            { adv lexbuf; FINALLY }
 | "break"              { adv lexbuf; BREAK }
 | "ret"                { adv lexbuf; RET }
 | "continue"           { adv lexbuf; CONTINUE }
 | "type"               { adv lexbuf; TYPE }
 | "struct"             { adv lexbuf; STRUCT }
 | "if"                 { adv lexbuf; IF }
 | "elif"               { adv lexbuf; ELIF }
 | "else"               { adv lexbuf; ELSE}
 | "case"               { adv lexbuf; CASE }
 | "default"		{ adv lexbuf; DEFAULT }
 | "goto"               { adv lexbuf; GOTO }
 | "for"                { adv lexbuf; FOR }
 | "from"		{ adv lexbuf; FROM }
 | "to"			{ adv lexbuf; TO }
 | "by"			{ adv lexbuf; BY }
 | "while"              { adv lexbuf; WHILE }
 | "until"		{ adv lexbuf; UNTIL }
 | "pass"               { adv lexbuf; PASS }
 | "NULL"               { adv lexbuf; NULL }
 | "^"                  { adv lexbuf; PTR }
 | "asm"                { adv lexbuf; ASM }
 | "use"                { adv lexbuf; USE }
 | "import"             { adv lexbuf; IMPORT }
 | "package"            { adv lexbuf; PACKAGE }
 | "export"             { adv lexbuf; EXPORT }
 | "uint8"|"byte"|"bool"|"char"	{ adv lexbuf; UINT8 }
 | "int8"		{ adv lexbuf; INT8 }
 | "uint16"		{ adv lexbuf; UINT16 }
 | "int16"		{ adv lexbuf; INT16 }
 | "uint32"|"uint"	{ adv lexbuf; UINT32 }
 | "int32"|"int"	{ adv lexbuf; INT32 }
 | "word"		{ adv lexbuf; WORD }
 | "uint64"|"ulong"	{ adv lexbuf; UINT64 }
 | "int64"|"long"	{ adv lexbuf; INT64 }
 | "float32"|"float"	{ adv lexbuf; FLOAT32 }
 | "float64"|"double"	{ adv lexbuf; FLOAT64 }
 | "string" 		{ adv lexbuf; STR }
 | "func"		{ adv lexbuf; FUNC }
 | "true"		{ adv lexbuf; INT( Int64.one ) }
 | "false"		{ adv lexbuf; INT( Int64.zero ) }
 | "class"		{ adv lexbuf; CLASS }
 | "method"		{ adv lexbuf; METHOD }
 | "self"		{ adv lexbuf; SELF }
 | "super"		{ adv lexbuf; SUPER }
 | "func"		{ adv lexbuf; FUNC }
 | "global"		{ adv lexbuf; GLOBAL }
 | "const"		{ adv lexbuf; CONST }

 | "$"			{ adv lexbuf; lcomment lexbuf; ENDLN }
 | "{-"			{ adv lexbuf; inComment := !inComment + 1; bcomment lexbuf; ENDLN }
 | id			{ adv lexbuf; ID( gs lexbuf ) }
 | eof			{ raise Eof }
 | _			{ adv lexbuf; 
                          error ("Invalid token: " ^ (gs lexbuf)); 
			  raise Lexer_error;
			  token lexbuf }


(* We have to do a few evil string-permutations to make it parse right... *)
 (* This bit grabs any sequence ending with ", not containing " but possibly
    containing \".  Wow, it actually works!  ^_^  silly comment junk->" *)

and grabstr = parse
   '"'			{ adv lexbuf; token lexbuf }
 | ("\\\""|[^'"'])*'"'	{ 
   			adv lexbuf; 
			STRING( parseStr 
			         (String.sub (gs lexbuf) 
			                     0 
				             ((String.length 
					        (gs lexbuf)) - 1) )) } 

and lcomment = parse
   '\n'			{ nl (); token lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }

and bcomment = parse
   "{-"			{ adv lexbuf; inComment := !inComment + 1; 
                          bcomment lexbuf }
 | "-}"			{ adv lexbuf; inComment := !inComment - 1; 
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 
