module Ty = Types
module Sym = Symbol

type t = Ty.ty

type access = unit

type enventry =
  | VarEntry of { ty : Ty.ty; read_only : bool }
  | FunEntry of { formals : Ty.ty list; result : Ty.ty }

let base_tenv =
  let add_to_table (s, t) table = Sym.enter table (Sym.symbol s) t in
  let to_add = [("int", Ty.INT); ("string", Ty.STRING)] in
  List.fold_right add_to_table to_add Sym.empty

let base_venc =
  let add_to_table (s, t) table = Sym.enter table (Sym.symbol s) t in
  let to_add = [
    ("print", FunEntry { formals=[Ty.STRING]; result=Ty.UNIT });
    ("flush", FunEntry { formals=[]; result=Ty.UNIT });
    ("getchar", FunEntry { formals=[]; result=Ty.UNIT });
    ("ord", FunEntry { formals=[Ty.STRING]; result=Ty.INT });
    ("chr", FunEntry { formals=[Ty.INT]; result=Ty.STRING });
    ("size", FunEntry { formals=[Ty.STRING]; result=Ty.INT });
    ("substring", FunEntry { formals=[Ty.STRING; Ty.INT; Ty.INT]; result=Ty.STRING });
    ("concat", FunEntry { formals=[Ty.STRING; Ty.STRING]; result=Ty.STRING });
    ("not", FunEntry { formals=[Ty.INT]; result=Ty.INT });
    ("exit", FunEntry { formals=[Ty.INT]; result=Ty.UNIT });
  ] in
  List.fold_right add_to_table to_add Sym.empty
