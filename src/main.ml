open Tiger
open Frame
open Codegen

let canonize stm =
  stm |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule

let codegen_frag frag =
  match frag with
  | Frame.PROC { body; frame } ->
    let stms = canonize body in
    let result = List.map (RiscVGen.codegen frame) stms
                 |> List.flatten in
    let Frame.{ prolog; body; epilog } = Frame.procEntryExit3 (frame, result) in
    if List.length body <> 0 then
      let label = List.hd body in
      (match label with Assem.LABEL _ -> () | _ -> ErrorMsg.impossible "body doesn't start LABEL");
      label :: prolog @ (List.tl body) @ epilog
      |> List.map (Assem.format Frame.string_of_register)
    else
      (prolog @ body @ epilog)
      |> List.map (Assem.format Frame.string_of_register)
  | Frame.STRING (label, str) ->
    [Printf.sprintf "%s:\n\t.ascii \"%s\"\n"
       (Symbol.name label) str]

let codegen_exp frame exp =
  canonize (Translate.unNx exp)
  |> List.map (RiscVGen.codegen frame)
  |> List.flatten
  |> List.map (Assem.format Frame.string_of_register)

let codegen filename =
  Parse.parse filename
  |> fun ast ->
  FindEscape.findEscape ast;
  Absyn.show_exp ast |> Printf.eprintf "%s\n";
  ast
  |> Semant.transProg
  |> fun (exp, frag) ->
  Printf.eprintf "================ IR ================\n";
  Translate.show_exp exp |> Printf.eprintf "%s\n";
  Printf.eprintf "=============== frags ==============\n";
  List.iter (fun f -> Frame.show_frag f |> Printf.eprintf "%s\n") frag;
  let frame =
      Frame.newFrame { name = Temp.namedlabel "main"; formals = [] }
  in
  List.fold_left (fun acc frag -> acc @ codegen_frag frag) [] frag
  @ (codegen_exp frame exp)

let run filename =
  try
    let instrs = codegen filename in
    Printf.eprintf "============ assembly ============\n";
    List.iter (Printf.eprintf "%s") instrs
  with ErrorMsg.SemanticError s as e ->
    Printf.eprintf "Failed to typecheck \"%s\"." filename;
    raise e

let main () = run Sys.argv.(1)
let () = main ()
