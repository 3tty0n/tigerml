open Types
open Env
open Semant

let lexbuf out (l : Lexing.lexbuf) =
  Parser.main Lexer.token l

let do_type_check absyn =
  Semant.trans_prog absyn;
  absyn

let from_string str =
  Lexing.from_string str
  |> lexbuf stdout
  |> do_type_check
  |> Absyn.show_exp
  |> print_string

let from_file f =
  let ic = open_in f in
  Lexing.from_channel ic
  |> lexbuf stdout
  |> do_type_check
  |> Absyn.show_exp
  |> print_string

let test_string = fun () ->
  let test1 = "if 3 >= 3 then 4 else 3" in
  let test2 = "let function f(x: int) : int = x in f(1) end" in
  let test3 = "let var N := 1 in N end" in
  from_string test1; print_newline ();
  from_string test2; print_newline ();
  from_string test3; print_newline ()

let exec_from_file _ =
  let argv = Sys.argv in
  if Array.length argv < 2 then
    failwith "please specify the name of a target file"
  else begin
    print_newline ();
    from_file (argv.(1))
  end

let () =
  (* test_string (); *)
  exec_from_file ()
