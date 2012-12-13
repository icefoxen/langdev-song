
let files = ["util"; "errorReport"; "syntree"; "songparse"; "songlex";
"symtable"; "semant"; "songc"];;

let makeinterfaces () =
   let rec loop = function
      [] -> ()
    | a :: b ->
       let fn = a ^ ".ml" 
       and fni = a ^ ".mli" in
       if Sys.file_exists fn then
          ignore (Sys.command ("ocamlc -c -i " ^ a ^ ".ml > " ^ fni)); 
       loop b;
   in loop files
;;

let dotools () =
   ignore (Sys.command "ocamlyacc -v *.mly");
   ignore (Sys.command "ocamllex *.mll");
;;

let compilefiles () = 
   let rec loop = function
      [] -> ()
    | a :: b ->
       let fn = a ^ ".ml"
       and fni = a ^ ".mli" in
       if Sys.file_exists fni then 
          ignore (Sys.command ("ocamlc -c " ^ fni));
       if Sys.file_exists fn then
          ignore (Sys.command ("ocamlc -c " ^ fn)); 
       loop b;
   in loop files
;;

let _ = 
   makeinterfaces ();
   dotools ();
   compilefiles ();
;;
