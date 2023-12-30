let fileName = ref ""
let lineNum = ref 1
let linePos = ref [ 0 ]

let reset () =
  fileName := "";
  lineNum := 1;
  linePos := [ 0 ]

exception TigerError of string
exception SemanticError of string

let error pos (msg : string) =
  let rec look (lst, n) =
    match lst with
    | a :: rest ->
        if a <= pos then
          ":" ^ string_of_int n ^ "." ^ string_of_int (pos - a + 1)
        else look (rest, n - 1)
    | _ -> ":0.0"
  in
  raise
    (TigerError
       (Printf.sprintf "%s%s:%s" !fileName (look (!linePos, !lineNum)) msg))

let error_semant pos msg =
  let rec look (lst, n) =
    match lst with
    | a :: rest ->
        if a <= pos then
          ":" ^ string_of_int n ^ "." ^ string_of_int (pos - a + 1)
        else look (rest, n - 1)
    | _ -> ":0.0"
  in
  raise
    (SemanticError
       (Printf.sprintf "%s%s:%s" !fileName (look (!linePos, !lineNum)) msg))

let fail msg = raise @@ TigerError msg

let impossible msg =
  raise (TigerError (Printf.sprintf "Error: Compiler bug: %s\n" msg))
