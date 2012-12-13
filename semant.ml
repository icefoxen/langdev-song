(* typecheck.ml
   Semantic checking.
   Checks types, existance of vars, number of function args,
   and other "fun" things like that.
   I don't know what I'm doing, and this bit uses a lot of stuff from
   Syntree and Symtable, so expect more than the usual amount of 
   documentaiton.

   Simon Heath
   20/10/2004
*)

exception TypeError of string;;
exception NameError of string;;


let loadInterface pkg syntree = 
  ()
;;

(* This goes through the file system and loads a package interface into
   the given structure.  It does NOT resolve dependancies recursively.
   ...grar.

   ...well, it basically goes like this: you load the interface file,
   stick all the functions and types into the package sub-structure,
   add it to the dep list, then go through and recursively load all it's 
   dependancies and add THEM to the dep list also.  One doesn't need
   to type-check the interface files, since presumably they are all
   perfect.
*)
let loadpkg pkg str =
  if Symtable.pkgexists str then
    let lexbuf = Lexing.from_channel (open_in (str ^ ".mli"))  in
    let parsetree = Songparse.main Songlex.start lexbuf in
      loadInterface pkg parsetree
  else
    raise (Symtable.PackageException 
	     (Printf.sprintf
		"Package %s uses package %s, which does not exist" 
		pkg.Symtable.pname str))
;;
  
let adddep pkg str =
  pkg.Symtable.pdeps <- str :: pkg.Symtable.pdeps
;;

let openpkg pkg str =
  adddep pkg str;
  loadpkg pkg str
;;
    

let usepkg pkg str =
  adddep pkg str;
  let p = Hashtbl.find pkg.Symtable.puses str in
    loadpkg p str
;;




(* So... this is going to take a couple passes.
   We go down the list of initial statements, and do this:
   For function defs, we add the function to the symbol table.
   For global and const var defs, we add the var.
   For type defs, we add the type.
   For imports, we load the interface into the local package.
   For uses, we load the interface into a different package.

   Then we go through all the function defs and do this:
   We push a scope.
   For local var defs, we add the var.
   For each statement, we recurse, pushing a scope if necessary.
   For each expression, we typecheck.
   For each instance of a var, we make sure it exists and we typecheck.
   For each instance of a function call, we make sure it exists and 
   that the number of args is correct, then we typecheck.
   For each instance of an array ref, we do what static checking we can.
   For each instance of a struct ref, we make sure the members exist.
   For each instance of an assignment, we typecheck, and handle multiple
   assignments.
   For each instance of a break or continue statement, we make sure we're
   inside a loop.
   For each instance of a pointer ref, we typecheck.
   We pop a scope.
*)

(* Checks to see if expr returns a type compatable with tp 
   tp is a Syntree.typestm and expr is a Syntree.stm
   A type is compatable if it is a number type with an equal or greater
   size and the same sign, or if it is a func with the same arg types
   (which is always true, for now), if it is an array or pointer of
   the same type, or (eventually) if it is a superclass of the given type.
   NIL fits into any type, any value fits into ^NONE
*)
let typeFits tp expr =
  true
;;

(* Same as above, but vr is a Syntree.var instead and it makes sure
   the default value is compatable.
*)
let typeFitsVar vr =
  true
;;



(* First pass of the semantic analyzer: recognizing declerations.
   smtbl is the symbol-table (Symtable.pkgsym), syntree is the syntax 
   tree (Syntree.moduleStruct)
*)
let firstPass smtbl sntree =
  (* Load imported vals into the symtable *)
  (* XXX: ... *)

  (* Load consts into the symtable *)
  let doConsts cst =  (* cst is a Syntree.var *)
    if Symtable.varexists smtbl cst.Syntree.vname then
      raise (NameError 
	       ("Constant " ^ cst.Syntree.vname ^ " is declared twice!"))
    else if typeFitsVar cst then
      let vrsym = {
	Symtable.vname = cst.Syntree.vname;
	Symtable.vsymtype = cst.Syntree.vtype;
	Symtable.vdefval = cst.Syntree.vval;
	Symtable.valloc = Symtable.Const;
      } in
	Symtable.addvar smtbl vrsym
  in
    List.iter doConsts sntree.Syntree.constlst;

  (* Load globals into the symtable *)
  let doGlobals gbl = 
    if Symtable.varexists smtbl gbl.Syntree.vname then
      raise (NameError ("Global " ^ gbl.Syntree.vname ^ " is declared twice!"))
    else if typeFitsVar gbl then
      let vrsym = {
	Symtable.vname = gbl.Syntree.vname;
	Symtable.vsymtype = gbl.Syntree.vtype;
	Symtable.vdefval = gbl.Syntree.vval;
	Symtable.valloc = Symtable.Global;
      } in
	Symtable.addvar smtbl vrsym
  in
    List.iter doGlobals sntree.Syntree.globallst;

  (* Load functions into the symtable *)
  let doFuncs fnc =
    if Symtable.varexists smtbl fnc.Syntree.fname then
      raise (NameError 
	       ("Function " ^ fnc.Syntree.fname ^ " is declared twice!"))
    else
      let fnsym = {
	Symtable.vname = fnc.Syntree.fname;
	Symtable.vsymtype = Syntree.Basictype( Syntree.FUNC( fnc.Syntree.fargs, fnc.Syntree.frettype ) );
	Symtable.vdefval = Syntree.Lambda( fnc.Syntree.fbody );
	Symtable.valloc = Symtable.Const
      } in
	Symtable.addvar smtbl fnsym
  in
    List.iter doFuncs sntree.Syntree.funlst;

  (* Load type defs into the symtable *)
  let doTypes tp =
    match tp with
	Syntree.Typedecl( nm, tpstm ) -> 
	  if Symtable.typeexists smtbl nm then
	    raise (NameError ("Type " ^ nm ^ " is declared twice!"))
	  else 
	      Symtable.addtype smtbl tp
      | Syntree.Structdecl( nm, vrlst ) ->
	  if Symtable.typeexists smtbl nm then
	    raise (NameError ("Struct " ^ nm ^ " is declared twice!"))
	  else
	      Symtable.addtype smtbl tp
  in
    List.iter doTypes sntree.Syntree.typelst;
;;


