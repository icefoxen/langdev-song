(* This is the main driver of the compiler.
   Basically, it just invokes one phase after another.
*)

let compileFile fn =
  ErrorReport.reset fn;
  try
    (* Lex and parse *)
    let lexbuf = Lexing.from_channel (open_in fn)  in
    let parsetree = Songparse.main Songlex.start lexbuf in
      (* Output .sgi file *)
      Syntree.outchan := open_out (fn ^ "i");
      Syntree.dumpModule parsetree;
      close_out !Syntree.outchan;
      (* Do semantic checking *)
      let symtbl = Symtable.createPkg fn fn in
      Typecheck.firstPass symtbl parsetree;
      print_endline "Stuff succeeded!";

  with
      Sys_error a -> ErrorReport.fatalError "File does not exist: %s\n" a
(*    | Songlex.Eof -> Printf.printf "Parsing successful!  Yay!\n" *)
    | Parsing.Parse_error -> 
	ErrorReport.error "Fatal parse error: Bad programmer, no cookie!"
;;

let usage () = 
  print_endline "Usage: testy filename.sg"
;;


let _ =
  
  let fn = Sys.argv.(1) in
    compileFile fn
  
;;

