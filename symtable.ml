(* symtable.ml
   Symbol table stuff.

   Simon Heath
   20/10/2004
*)

exception PackageException of string

type alloctype =
   New
 | Const
 | Global

(* These are the types of various symbols.  There are three namespaces,
   really: vars/functions, packages, and types.
   Mrrrrrrf...  functions are just variables.  But then they need real,
   strong types.  Erm, maybe they should just be a special case at the
   moment?  ^_^;;
   Dammit though, what about function signatures though?  Either we ignore
   them, like C, and have potentially REALLY nasty things happen, or we
   state them explicitly, like D, and do a lot of parser re-engineering.
   The IDEAL solution would be to infer the type automatically, like
   Ocaml.  Can we even do that?  No, because if we pass a function to
   another function, we have no way of knowing what the latter expects.  So
   it all breaks down.
   So we ARE going to need some way of explicitly telling the signature
   of a func type.  Even Ocaml and Haskell need to do that at times.  
   Dammit.  I should have seen this coming.  Leaving weak spaces in your
   spec does not work well.
   So...  For now, any var or arg declared type "func" is really declared
   type "func (args ^NONE : ^NONE)".  That should cover just about any
   eventuality...  Well, except multiple returns, but you can't have
   everything.  At least not yet.
*)
type varsym = {
  mutable vname : string;
  mutable vsymtype : Syntree.typestm;
  mutable vdefval : Syntree.stm;
  mutable valloc : alloctype;
}




type pkgsym = {
  mutable pname : string;
  mutable pfilename : string;
  mutable pvars : (string, varsym) Hashtbl.t;
  mutable ptypes : (string, Syntree.typedecl) Hashtbl.t;
  mutable pdefs : (string, string) Hashtbl.t;
  mutable pdeps : string list;
  mutable puses : (string, pkgsym) Hashtbl.t;
}


let createPkg nm filenm = {
  pname = nm;
  pfilename = filenm;
  pvars = Hashtbl.create 32;
  ptypes = Hashtbl.create 8;
  puses = Hashtbl.create 8;
  pdefs = Hashtbl.create 8;
  pdeps = [];
}
;;



(* Hrm.  How exactly are packages going to work? 
   We're going to need different symbol tables for different packages...
   Ah, that's easy then.  Each pkgsym just refers to it's own private
   packages.  ^_^
   Hmm though, that allows building trees of dependancies, which should
   be handled more gracefully since each dependancy should only be included 
   once, and also allows building circular dependancies, which should
   be taken into account.
   So, we care about the current package we're compiling, and all the packages
   it opens/uses.  We care about the contents of those packages, and we need
   to know which packages THEY depend on, but we do NOT need to know about
   the contents of thehe dependancies.  Hmmm...
*)

(* Scope stack.  Basically a list of lists; the first cons is the top.
   Each time a new symbol is added in the current scope, it is added to the
   list at the top of the stack.  When a scope is popped, it goes through
   the top list and removes all the names from the vartable.
*)
let scopestack = ref [];;





let getvar pkg str = 
  Hashtbl.find pkg.pvars str
;;

let addvar pkg vl = 
  Hashtbl.add pkg.pvars vl.vname vl;
  match !scopestack with
      a :: b -> scopestack := (vl.vname :: a) :: b
    | [] -> scopestack := [[vl.vname]]
;;

let remvar pkg str =
  Hashtbl.remove pkg.pvars str
;;

let varexists pkg str = 
  Hashtbl.mem pkg.pvars str
;;



let pushscope pkg = 
  scopestack := [] :: !scopestack

let popscope pkg = 
  let rec loop = function
      [] -> ()
    | hd :: tl -> remvar pkg hd; loop tl;
  in
    match !scopestack with
	hd :: tl -> scopestack := tl; loop hd
      | [] -> scopestack := []
;;

(* This should check through the actual filesystem looking for packages *)
(* XXX: for the moment, packages only exist in the local dir. *)
let pkgexists str =
  Sys.file_exists (str ^ ".mli")
;;


let getTypeName = function
    Syntree.Typedecl( n, _ ) -> n
  | Syntree.Structdecl( n, _ ) -> n
;;

let addtype pkg tp =
  Hashtbl.add pkg.ptypes (getTypeName tp) tp
;;

let gettype pkg str =
  Hashtbl.find pkg.ptypes str
;;


let typeexists pkg str =
  Hashtbl.mem pkg.ptypes str
;;


let adddef pkg str =
  Hashtbl.add pkg.pdefs str str
;;

let hasdef pkg str =
  Hashtbl.mem pkg.pdefs str
;;
