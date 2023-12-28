module T = Types
module S = Symbol

type access = unit
type ty = T.ty
[@@deriving show]
type enventry = VarEntry of {access : Translate.access; ty: ty; const: bool}
              | FunEntry of {level : Translate.level; label : Temp.label; formals: ty list; result: ty}
[@@deriving show]

let base_tenv : ty S.table =
  let base_types = [
    ("int", T.INT);
    ("string", T.STRING)
  ] in
  List.fold_left (fun tbl (name, el) -> S.enter tbl (S.symbol name) el)
    S.empty
    base_types

let base_venv : enventry S.table =
  let base_funcs =
    [
        ("print", [ ("s", Types.STRING) ], Types.UNIT);
        ("flush", [], Types.NIL);
        ("getchar", [], Types.STRING);
        ("ord", [ ("s", Types.STRING) ], Types.INT);
        ("chr", [ ("i", Types.INT) ], Types.STRING);
        ("size", [ ("s", Types.STRING) ], Types.INT);
        ( "substring",
          [ ("s", Types.STRING); ("first", Types.INT); ("n", Types.INT) ],
          Types.STRING );
        ("concat", [ ("s1", Types.STRING); ("s2", Types.STRING) ], Types.STRING);
        ("not", [ ("i", Types.INT) ], Types.INT);
        ("exit", [ ("i", Types.INT) ], Types.UNIT);
      ]
  in
  let make_fun_entry (name, formals, result) =
    let label = Temp.namedlabel name in
    (S.symbol name,
     FunEntry {
       level=Translate.newLevel { parent=Translate.outermost;
                                  formals=List.map (fun _ -> false) formals;
                                  name=label
                                };
       label=Temp.newlabel ();
       formals=List.map (fun (_, t) -> t) formals;
       result=result
     })
  in
  List.fold_left (fun tbl (name, el) -> S.enter tbl name el)
    S.empty
    (List.map make_fun_entry base_funcs)
