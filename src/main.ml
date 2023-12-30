open Tiger
open Frame

let canonize stm =
  stm |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule

let run filename =
  try
    let ast = Parse.parse filename in
    let _ = FindEscape.findEscape ast in
    (* print_string (Absyn.show_exp ast); print_newline (); *)
    let exp, frag = Semant.transProg ast in
    Printf.eprintf "================ IR ================\n";
    Printtree.printtree (stderr, Translate.unNx exp);
    Printf.eprintf "=============== frags ==============\n";
    List.iter (fun f -> Frame.show_frag f |> Printf.eprintf "%s\n") frag
  with ErrorMsg.SemanticError s as e ->
    Printf.eprintf "Failed to typecheck \"%s\"." filename;
    raise e

let main () = run Sys.argv.(1)
let () = main ()
