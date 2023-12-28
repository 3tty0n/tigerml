let run filename =
  try
    let ast = Parse.parse filename in
    (* let _ = FindEscape.findEscape ast in *)
    print_string (Absyn.show_exp ast); print_newline ();
    let exp = Semant.transProg ast in
    print_string (Translate.show_exp exp)
  with Semant.SemanticError -> print_endline ("Failed to typecheck \"" ^ filename ^ "\"."); exit 1

let main () = run (Sys.argv.(1))

let () = main ()
