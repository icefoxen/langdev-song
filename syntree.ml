(* syntree.ml
   Song abstract syntax tree.

   Needs to be able to be decorated...
   Screwit, that's what the symbol table ish for.

   nodes:
   module importdecl usedecl globaldecl constdecl fundecl typedecl structdecl
   versiondecl exportdecl name typename ptr addr vartype array int float
   char str arrayliteral arg fundecl vararg ifstm whilestm forstm fromstm
   trystm gotostm label versionstm varstm freestm asmstm assignstm retstm
   breakstm continuestm funcall ptrref primop coerce sizeof arrayref structref
   allocator

   ...This whole thing is a rather ugly kludge, and I hate it.
   However, it seems to do it's job, and probably won't be THAT hard to
   change down the road.  And if it still runs... drive it, baby!

   XXX: Each node should probably include a line number value,
   for error messages...

   Simon Heath
   07/08/2004
*)

exception DumpError of string


(* I don't actually use this, but I really could with minor changes that I
   don't want to bother making.  As it is, it serves as a nice example of
   how to turn a list of strings into a single string.  You will see it 
   again, with minor variations. *)
let strjoin strlst delim =
  let rec loop strlst accm =
    match strlst with
	a :: b -> loop b (accm ^ (if accm = "" then "" else delim) ^ a)
      | [] -> accm
  in
    loop strlst ""
;;


(* XXX: HMMMM.  Functions are just the same as variables... *)
type moduleStruct = {
  mutable name       : string;
  mutable importlst  : string list;
  mutable uselst     : string list;
  mutable exportlst  : string list;
  mutable constlst   : var list;
  mutable globallst  : var list;
  mutable typelst    : typedecl list;
  mutable funlst     : fundecl list;
  mutable versionlst : string list;
}

and pkgref = string * string

and var = {
  (* Should the name be a pkgref? *)
  vname : string;
  vtype : typestm;
  vval  : stm;
}

and typeident =
    INTSIZE of int
  | UINTSIZE of int
  | FLOATSIZE of int
  | BYTE
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | UBYTE
  | USHORT

  | UINT
  | ULONG
  | CHAR
  | STR
  | BOOL
  | NONE
  | FUNC of funarg list * typestm list



and typestm =
    (* Basictype( name, size ) *)
    Basictype of typeident
  | Ptrtype of typestm 
    (* Arraytype( name, size ) *)
  | Arraytype of typestm
      (* Varienttype size is always 4 --pointer *)
      (*  | Varienttype of typestm * typestm list *)
  | Customtype of pkgref
  | Nulltype

and typedecl =
    Typedecl of string * typestm
  | Structdecl of string * var list

(* varname, vartype, default value, is it a varlength arg? 
   No support for default values... yet.
*)
and funarg = (string * typestm * stm * bool)

and fundecl = {
  fname     : string;
  frettype  : typestm list;
  fargs     : funarg list;
  fbody     : stm list;
}


(* Not, perhaps, the tidiest way to do it, buuuuut... *)
and stm = 
    (* Ifstm( condition, body, [(elifcondition, elifbody)], elsebody ) *)
    Ifstm of stm * stm list * (stm * stm list) list * stm list
    (* Forstm( var, condition, increment, body ) *)
  | Forstm of stm * stm * stm * stm list
  | Whilestm of stm * stm list
    (* Fromstm( var, to, by, body ) *)
  | Fromstm of var * stm * stm * stm list
    (* Trystm( trybody, withid, withbody, finallybody ) *)
  | Trystm of stm list * string * stm list * stm list
  | Gotolabel of string
  | Gotostm of string
  | Versionstm of string * stm list
  | Vardeclstm of var list * stm list
  | Freestm of pkgref
  | Asmstm of string
  | Assignstm of stm list * stm list
  | Idstm of string
  | Strstm of string
  | Intstm of int
  | Floatstm of float
  | Charstm of char
  | Retstm of stm list
  | Breakstm
  | Continuestm
  | Coercestm of typestm * stm
  | Sizeofstm of typestm
  | Arraystm of stm * stm
    (* The string list ain't exactly ideal...  a stm * string cons would be
       better.  However, it appears to be almost impossible to parse, let
       alone validate.  So. *)
  | Structstm of stm * string list
    (* Allocator types aren't really necessary anymore, but they do no harm
       and I may end up putting them back in later, so.   *)
  | Allocstm of allocator * typestm * stm
  | Primop of string * stm list
  | Funcall of pkgref * stm list
  | Fundeclstm of fundecl
  | Nullstm
  | Ptrstm of stm
(*  | Addrstm of stm *)
  | Arraylitstm of stm list
  | Valstm of pkgref 
  (* Erm, we'll need this later...  We also need it for semantic
     checking *)
  | Lambda of stm list

and allocator = NEW | STATIC (* | ALLOC *)
;;

let makevar nm tp vl = { vname = nm; vtype = tp; vval = vl };;

let makefun hdr body =
  match hdr with
      nm, rgs, rttype -> { 
	fname = nm; fargs = rgs; frettype = rttype; fbody = body
      }
;;




(************************************************************************)
(* All of the following are UNPARSER functions, used to take a syntax tree
   and dump it back out into an interface file containing all external
   declerations of globals, constants, functions, etc.  This is used
   for the package system.
*)

let outchan = ref stdout;;

(* Don't try to find meaning in these three function names;
   there isn't any. *)
let p x y = Printf.sprintf x y;;
let pout x y = Printf.fprintf !outchan x y;;

let dpout x = Printf.fprintf !outchan x;;



let rec dumpModule m =
  pout "module %s;\n" m.name;
  List.iter (fun x -> pout "import %s;\n" x) m.importlst;
  List.iter (fun x -> pout "use %s;\n" x) m.uselst;
  List.iter (fun x -> pout "export %s;\n" x) m.exportlst;
  List.iter (fun x -> pout "def %s;\n" x) m.versionlst;
  List.iter (fun x -> pout "%s\n" (dumpConst x)) m.constlst;
  List.iter (fun x -> pout "%s\n" (dumpGlobal x)) m.globallst;
  List.iter (fun x -> pout "%s\n" (dumpTypeDecl x)) m.typelst;
  List.iter (fun x -> pout "%s\n" (dumpFunheader x)) m.funlst;

and dumpPkgref = function
    a, b -> if a = "" then b else p "%s::%s" a b

and dumpVar v =
  p "%s %s <- %s" v.vname (dumpTypestm v.vtype) (dumpVal v.vval)

and dumpTypestm = function
    Basictype( typeid )  -> (dumpTypeident typeid)
  | Ptrtype( typestm ) -> p "^%s" (dumpTypestm typestm)
  | Arraytype( typestm ) -> p "[%s]" (dumpTypestm typestm)
(*  | VarientType( s ) -> s *)
  | Nulltype -> ""
  | Customtype( pkg, name ) -> p "%s::%s" pkg name

(* This assumes that a byte is always 8 bits, but I think that's a safe 
   assumption.  Is someone really going to port this to VAX? *)
and dumpTypeident = function
    INTSIZE( i )   -> "int" ^ (string_of_int (i * 8))
  | UINTSIZE( i )  -> "uint" ^ (string_of_int (i * 8))
  | FLOATSIZE( i ) -> "float" ^ (string_of_int (i * 8))
  | BYTE           -> "byte"
  | SHORT          -> "short"
  | INT            -> "int"
  | LONG           -> "long"
  | FLOAT          -> "float"
  | DOUBLE         -> "double"
  | UBYTE          -> "ubyte"
  | USHORT         -> "ushort"
  | UINT           -> "uint"
  | ULONG          -> "ulong"
  | CHAR           -> "char"
  | STR            -> "str"
  | BOOL           -> "bool"
  | FUNC( _, _ )   -> "func"
  | NONE           -> "NONE"

(* Not the cleanest... rather silly, really... but it works. *)
and dumpTypeDecl = function
    Typedecl( str, typestm ) -> p "type %s = %s;\n" str (dumpTypestm typestm)
  | Structdecl( str, varlst ) ->
      let strjoin vlst delim =
	let rec loop vlst accm =
	  match vlst with
	      a :: b -> loop b (accm ^ (if accm = "" then "" else delim) ^
				(dumpVar a))
	    | [] -> accm
	in
	  loop varlst ""
      in
	 p "struct %s =\n  %s;;\n" str ((strjoin varlst ";\n  ") ^ ";\n")



and dumpVal = function
    Intstm( i ) -> p "%d" i
  | Floatstm( f ) -> p "%f" f
  | Charstm( c ) -> p "'%c'" c
  | Strstm( s ) -> p "\"%s\"" s
  | Valstm( v ) -> dumpPkgref v
  | Arraylitstm( a ) -> (* a is a value list *)
      (* XXX: Change this if ARRAYSTART and ARRAYEND change! *)
      p "{!%s!}" (List.fold_left (fun str vl -> str ^ ", " ^ (dumpVal vl))
		    "" a)
  | Nullstm -> p "%s" "NIL";
  | _ -> raise (DumpError "dumpVal: Passed a statement that's not a val!")


and dumpGlobal g =
  p "global %s;\n" (dumpVar g)

and dumpConst c =
  p "const %s;\n" (dumpVar c)


(* XXX: Changed the type for fun parameters to be able to do var-length
   args.  Figure out how that's gonna work...
*)
and dumpFunArg a =
  match a with
      vname, vtype, vval, isvarlen ->
	if isvarlen then
	  p "... %s" (dumpTypestm vtype)
	else
	  p "%s %s" vname (dumpTypestm vtype)

and dumpFunheader f =
  let joinfuns vlst delim =
    let rec loop vlst accm =
      match vlst with
	  a :: b -> loop b (accm ^ (if accm = "" then "" else delim) ^
			    (dumpFunArg a))
	| [] -> accm
    in
      loop vlst ""
  and joinrets vlst delim =
    let rec loop vlst accm =
      match vlst with
	  a :: b -> loop b (accm ^ (if accm = "" then "" else delim) ^
			    (dumpTypestm a))
	| [] -> accm
    in
      loop vlst ""
  in
  let args = joinfuns f.fargs ", " 
  and rettype = joinrets f.frettype ", "
  in
    (* The "if" is a small hack to check whether there's a return type 
       clause or not. *)
    p "%s %s %s %s = ;;\n" f.fname args (if rettype = "" then "" else ":") rettype



;;
