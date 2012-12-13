
let compileFile fn =
  ErrorReport.reset fn;
  try
    let lexbuf = Lexing.from_channel (open_in fn) in
    let parsetree = Songparse.main Songlex.token lexbuf in
      ignore parsetree; 
      print_endline "Parsing succeeded!";

  with
      Sys_error a -> (Printf.eprintf "File does not exist: %s\n" a; 
		    exit 1)
    | Parsing.Parse_error -> 
	ErrorReport.error "Fatal parse error: Bad programmer, no cookie!"
;;

let usage () = 
  print_endline "Usage: testy filename.shs"
;;


let _ =
  
  let fn = Sys.argv.(1) in
    compileFile fn
  
;;

