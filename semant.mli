type venv
type tenv
type senv
type env = {venv: venv; tenv: tenv; senv: senv}
type expty

val transVar: venv -> tenv -> senv -> Translate.level -> Absyn.var -> expty
val transExp: venv -> tenv -> senv -> Translate.level -> Absyn.exp -> expty
val transDec: venv -> tenv -> senv -> Translate.level -> Absyn.dec -> env * Translate.exp list
val transTy:          tenv -> Absyn.ty  -> Types.ty
val transProg:                Absyn.exp -> Translate.exp

exception SemanticError
