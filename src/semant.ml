module A = Absyn
module S = Symbol
module T = Types
module Tr = Translate

type venv = Env.enventry S.table
type tenv = T.ty S.table
type senv = { break : Temp.label option; num_locals : int }
type env = { venv : venv; tenv : tenv; senv : senv }
type expty = { exp : Tr.exp; ty : T.ty }

exception SemanticError

let base_senv = { break = None; num_locals = 0 }

(* string utils *)
let quote str = "\"" ^ str ^ "\""

let tyStr ty =
  (* first signifies if function is called for the first time *)
  let rec tyStr_ first ty =
    match ty with
    | T.INT -> "int"
    | T.STRING -> "string"
    | T.RECORD (lst, _) ->
        if first then
          match lst with
          | [] -> "{}"
          | hd :: tl ->
              let strForField (sym, ty) = S.name sym ^ ":" ^ tyStr_ false ty in
              let combine agg next = agg ^ "; " ^ next in
              let body =
                List.fold_left combine (strForField hd)
                  (List.map strForField tl)
              in
              "{" ^ body ^ "}"
        else "record"
    | T.ARRAY (ty, _) ->
        if first then "array of " ^ tyStr_ false ty else "array"
    | T.NIL -> "nil"
    | T.UNIT -> "unit"
    | T.NAME (name, tyref) -> (
        let prefix = "name(" ^ S.name name ^ ", " in
        match !tyref with
        | Some ty -> prefix ^ "some)"
        | None -> prefix ^ "none)")
  in

  tyStr_ true ty

let opStr = function
  | A.PlusOp -> "+"
  | A.MinusOp -> "-"
  | A.TimesOp -> "*"
  | A.DivideOp -> "/"
  | A.EqOp -> "="
  | A.NeqOp -> "<>"
  | A.LtOp -> "<"
  | A.LeOp -> "<="
  | A.GtOp -> ">"
  | A.GeOp -> ">="

(* helper functions *)
let error pos msg = ErrorMsg.error_semant pos msg
let checkErr () = ()
let return exp ty = { exp; ty }

let rec actualTy ty pos =
  match ty with
  | T.NAME (sym, reference) -> (
      match !reference with
      | Some resolved -> actualTy resolved pos
      | None ->
        error pos
          ("The type \"" ^ S.name sym ^ "\" has still not been resolved."))
  | _ -> ty

let checkInt ({ ty; _ }, pos) = actualTy ty pos = T.INT
let checkNil ({ ty; _ }, pos) = actualTy ty pos = T.NIL

let getValue venv sym pos =
  match S.look venv sym with
  | Some value -> value
  | None ->
      error pos ("The value \"" ^ S.name sym ^ "\" is undefined.")

let getType tenv sym pos =
  match S.look tenv sym with
  | Some typ -> typ
  | None ->
      error pos ("The type \"" ^ S.name sym ^ "\" is undefined.")

let getActualType tenv sym pos = actualTy (getType tenv sym pos) pos

let checkTy expected actual =
  match expected with
  | T.RECORD _ -> expected == actual || actual = T.NIL
  | _ -> expected == actual

let isConst venv var pos =
  match var with
  | A.SimpleVar (sym, _) -> (
      match getValue venv sym pos with
      | Env.VarEntry { const; _ } -> const
      | _ -> false)
  | _ -> false

let rec contains lst el =
  match lst with [] -> false | hd :: tl -> el = hd || contains tl el

(* main typechecking functions *)

(*  venv * tenv * Absyn.exp -> expty  *)
let rec transExp venv tenv senv level exp =
  let rec trexp = function
    | A.VarExp var -> transVar venv tenv senv level var
    | A.NilExp -> return (Translate.intExp 0) T.NIL
    | A.IntExp i -> return (Translate.intExp i) T.INT
    | A.StringExp (s, _) -> return (Translate.stringExp s) T.STRING
    | A.CallExp { func; args; pos } -> (
        match Symbol.look venv func with
        | Some (Env.FunEntry { formals; result; label; level = fn_level; _ }) ->
            let rec process_args params argments i acc =
              match (params, argments) with
              | [], [] -> acc
              | _, [] ->
                  error pos
                    ("Not enough arguments supplied to " ^ S.name func ^ ".")
              | [], _ ->
                  error pos
                    ("Too many arguments supplied to " ^ S.name func ^ ".")
              | phd :: ptl, ahd :: atl ->
                  let { exp = argExp; ty = argTy } = trexp ahd in
                  let actual_phd = actualTy phd pos in
                  if not (checkTy actual_phd argTy) then
                    error pos
                      ("Type of argument " ^ string_of_int i
                     ^ " to function call " ^ S.name func ^ " should be "
                     ^ tyStr actual_phd ^ " (" ^ tyStr argTy ^ " found).")
                  else process_args ptl atl (i + 1) acc @ [ argExp ]
            in

            let processed_args = process_args formals args 1 [] in
            return
              (Translate.callExp (label, fn_level, level, processed_args))
              (actualTy result pos)
        | _ -> error pos ("Undefined function " ^ S.name func))
    | A.OpExp { left; oper; right; pos } -> (
        let op = quote (opStr oper) in
        let isStringOp, isRecArrOp =
          match oper with
          | A.EqOp | A.NeqOp -> (true, true)
          | A.LtOp | A.LeOp | A.GtOp | A.GeOp -> (true, false)
          | _ -> (false, false)
        in
        let isIntOnlyOp = not isStringOp in
        let ({ ty = lty; _ } as lexpty) = trexp left in
        let ({ ty = rty; _ } as rexpty) = trexp right in
        let lTyStr = tyStr lty in
        let rTyStr = tyStr rty in
        let _ =
          if isIntOnlyOp then
            match (checkInt (lexpty, pos), checkInt (rexpty, pos)) with
            | false, false ->
                error pos ("Both operands to " ^ op ^ " are not integers.")
            | false, true ->
                error pos ("Left operand to " ^ op ^ " is not an integer.")
            | true, false ->
                error pos ("Right operand to " ^ op ^ " is not an integer.")
            | _ -> ()
          else if (not (checkTy lty rty)) && not (checkTy rty lty) then
            error pos
              ("Types of operands to " ^ op
             ^ " do not match. The left operand is " ^ lTyStr
             ^ "; the right is " ^ rTyStr ^ ".")
          else
            (* types are the same, verify that ops are valid for types *)
            match lty with
            | T.INT -> ()
            | T.STRING ->
                if not isStringOp then
                  error pos ("Cannot apply " ^ op ^ " to string operands.")
            | _ ->
                if not isRecArrOp then
                  error pos
                    ("Cannot apply " ^ op ^ " to record/string operands.")
        in
        match oper with
        | A.PlusOp | A.MinusOp | A.DivideOp | A.TimesOp ->
            return
              (Translate.arithmeticOperation (lexpty.exp, oper, rexpty.exp))
              T.INT
        | A.EqOp | A.NeqOp | A.LtOp | A.LeOp | A.GtOp | A.GeOp ->
            return
              (Translate.comparisonOperation (lexpty.exp, oper, rexpty.exp))
              T.INT)
    | A.RecordExp { fields; typ; pos } ->
        let typeName = S.name typ in
        (* ensure typ is a record type *)
        let ty, formalFields =
          match getActualType tenv typ pos with
          | T.RECORD (lst, _) as ty -> (ty, lst)
          | _ ->
              error pos (typeName ^ " is not a record type.")
        in
        (* ensure each field is instantiated with the right type, in the right order *)
        let rec checkFields formals actuals translated =
          match (formals, actuals) with
          | [], [] -> translated
          | (sym, _) :: _, [] ->
              error pos
                ("Missing field "
                ^ quote (S.name sym)
                ^ " in " ^ typeName ^ " instantiation.")
          | [], (sym, _, _) :: _ ->
              error pos
                ("Unknown field "
                ^ quote (S.name sym)
                ^ " in " ^ typeName ^ " instantiation.")
          | (fsym, fty) :: ftl, (asym, exp, _) :: atl ->
              let { ty = aTy; exp = aExp } = trexp exp in
              let actualFty = actualTy fty pos in
              let fname = quote (S.name fsym) in
              let aname = quote (S.name asym) in
              if fsym <> asym then
                error pos
                  ("Expected field " ^ fname ^ ", but found " ^ aname ^ " in "
                 ^ typeName ^ " instantiation.")
              else
                let { ty = aty; _ } = trexp exp in
                if not (checkTy actualFty aty) then
                  let ftystr = tyStr actualFty in
                  let atystr = tyStr aty in
                  error pos
                    ("Value of " ^ fname ^ " should be " ^ ftystr
                   ^ ", but found " ^ atystr ^ " in " ^ typeName
                   ^ " instantiation.")
                else checkFields ftl atl (translated @ [ aExp ])
        in
        let translated = checkFields formalFields fields [] in
        return (Translate.recordExp translated) ty
    | A.SeqExp [] ->
        (* handling unit, () *)
        return (Translate.intExp 0) T.UNIT
    | A.SeqExp lst ->
        let exps', ty =
          List.fold_left
            (fun (l, _) (e, _) ->
              let { exp; ty } = trexp e in
              (l @ [ exp ], ty))
            ([], Types.NIL) lst
        in
        return (Translate.seqExp exps') ty
    | A.AssignExp { var; exp; pos } ->
        let { exp = varExp; ty = varTy } = transVar venv tenv senv level var in
        let { exp = aExp; ty = expTy } = trexp exp in
        let varTyStr = tyStr varTy in
        let expTyStr = tyStr expTy in
        if isConst venv var pos then
          error pos "Cannot reassign a constant variable."
        else if not (checkTy varTy expTy) then
          error pos
            ("Cannot assign a value of type " ^ expTyStr
             ^ " to an lvalue of type " ^ varTyStr ^ ".")
        else return (Translate.assignExp (varExp, aExp)) T.UNIT
    | A.IfExp { test; then'; else'; pos } ->
        let ({ exp = testExp; ty = testTy } as testExpTy) = trexp test in
        let { exp = thenExp; ty = thenTy } = trexp then' in
        let testTyStr = tyStr testTy in
        let thenTyStr = tyStr thenTy in
        let _ =
          if not (checkInt (testExpTy, pos)) then
            error pos
              ("Condition in if-expression should have type int, not "
             ^ testTyStr)
        in
        let result =
          match else' with
          | Some exp ->
              let { exp = elseExp; ty = elseTy } = trexp exp in
              let elseTyStr = tyStr elseTy in
              if (not (checkTy thenTy elseTy)) && not (checkTy elseTy thenTy)
              then
                error pos
                  ("Types of then- and else-branches differ. "
                 ^ "Then-branch has type " ^ thenTyStr ^ "; "
                 ^ "else-branch has type " ^ elseTyStr ^ ".")
              else
                return (Translate.ifThenElse (testExp, thenExp, elseExp)) thenTy
          | None ->
              if thenTy <> T.UNIT then
                error pos
                  ("The body of an if-expression with no else-branch should be \
                    unit, not " ^ thenTyStr)
              else return (Translate.ifThen (testExp, thenExp)) thenTy
        in
        result
    | A.WhileExp { test; body; pos } ->
        let ({ exp = testExp; ty = testTy } as testExpTy) = trexp test in
        let end_label = Temp.newlabel () in
        let senv' = { senv with break = Some end_label } in
        let { exp = bodyExp; ty = bodyTy } =
          transExp venv tenv senv' level body
        in
        let testTyStr = tyStr testTy in
        let bodyTyStr = tyStr bodyTy in
        let _ =
          if not (checkInt (testExpTy, pos)) then
            error pos
              ("Condition in while loop should have type int, not " ^ testTyStr)
        in
        let _ =
          if bodyTy <> T.UNIT then
            error pos ("Body of while loop should return unit, not " ^ bodyTyStr)
        in
        return (Translate.whileExp (testExp, bodyExp, end_label)) T.UNIT
    | A.ForExp { var; escape; lo; hi; body; pos } ->
        let ({ exp = loExp; ty = loTy } as loExpTy) = trexp lo in
        let ({ exp = hiExp; ty = hiTy } as hiExpTy) = trexp hi in
        let loTyStr = tyStr loTy in
        let hiTyStr = tyStr hiTy in
        let access = Tr.allocLocal level !escape in
        let venv' =
          S.enter venv var (Env.VarEntry { access; ty = T.INT; const = true })
        in
        let _ =
          if not (checkInt (loExpTy, pos)) then
            error pos
              ("For loop lower bound should have type int, not " ^ loTyStr ^ ".")
        in
        let _ =
          if not (checkInt (hiExpTy, pos)) then
            error pos
              ("For loop upper bound should have type int, not " ^ hiTyStr ^ ".")
        in
        let end_label = Temp.newlabel () in
        let senv' = { senv with break = Some end_label } in
        let { exp = bodyExp; ty = bodyTy } =
          transExp venv' tenv senv' level body
        in
        let bodyTyStr = tyStr bodyTy in
        let _ =
          if bodyTy <> T.UNIT then
            error pos
              ("Body of for loop should have unit return type, not " ^ bodyTyStr
             ^ ".")
        in
        let counter_exp = Translate.simpleVar (access, level) in
        let for_exp =
          Translate.forExp (counter_exp, loExp, hiExp, bodyExp, end_label)
        in
        return for_exp T.UNIT
    | A.BreakExp pos -> (
        match senv.break with
        | Some end_label -> return (Translate.breakExp end_label) T.UNIT
        | None -> error pos "encountered break statement when not in loop")
    | A.LetExp { decs; body; pos } ->
        let combine ({ venv; tenv; senv }, exps) dec =
          let env, new_exps = transDec venv tenv senv level dec in
          (env, exps @ new_exps)
        in
        let { venv = venv'; tenv = tenv'; senv = senv' }, decExps =
          List.fold_left combine ({ venv; tenv; senv }, []) decs
        in
        let { exp = bodyExp; ty = bodyTy } =
          transExp venv' tenv senv level body
        in
        return (Translate.seqExp (decExps @ [ bodyExp ])) bodyTy
    | A.ArrayExp { typ; size; init; pos } ->
        let typeName = S.name typ in
        (* ensure typ is an array type *)
        let ty, arrTy =
          match getActualType tenv typ pos with
          | T.ARRAY (arrTy, _) as ty -> (ty, actualTy arrTy pos)
          | _ ->
              error pos (typeName ^ " is not an array type.")
        in
        let arrTyStr = tyStr arrTy in
        let ({ exp = sizeExp; ty = sizeTy } as sizeExpTy) = trexp size in
        let { exp = initExp; ty = initTy } = trexp init in
        let sizeTyStr = tyStr sizeTy in
        let initTyStr = tyStr initTy in
        (* ensure size has type int *)
        let _ =
          if not (checkInt (sizeExpTy, pos)) then
            error pos
              ("Size of array should have type int, not " ^ sizeTyStr ^ ".")
        in
        (* ensure initializing value has the correct type *)
        let _ =
          if not (checkTy arrTy initTy) then
            error pos
              ("Initializing value for array should have type " ^ arrTyStr
             ^ ", not " ^ initTyStr ^ ".")
        in
        return (Translate.arrayExp (sizeExp, initExp)) ty
  in
  trexp exp

(*  venv * tenv * Absyn.var -> expty  *)
and transVar venv tenv senv level var =
  match var with
  | A.SimpleVar (sym, pos) ->
      let varStr = S.name sym in
      let ty, access =
        match getValue venv sym pos with
        | Env.VarEntry { ty; access; _ } -> (actualTy ty pos, access)
        | _ -> error pos ("Function " ^ varStr ^ " is being used as a variable.")
      in
      return (Translate.simpleVar (access, level)) ty
  | A.FieldVar (recVar, sym, pos) ->
      let fieldStr = quote (S.name sym) in
      (* ensure that the lvalue is a record type *)
      let { exp = recVarExp; ty = recTy } =
        transVar venv tenv senv level recVar
      in
      let fieldList =
        match recTy with
        | T.RECORD (lst, _) -> lst
        | _ ->
          error pos
            ("Cannot access field " ^ fieldStr ^ " on a non-record type.")
      in
      let rec findField i = function
        | [] ->
            error pos ("Unknown field " ^ fieldStr ^ ".")
        | (fieldSym, fieldTy) :: fieldTl ->
            if fieldSym = sym then (i, actualTy fieldTy pos)
            else findField (i + 1) fieldTl
      in
      let offset, fieldTy = findField 0 fieldList in
      return (Translate.fieldVar (recVarExp, offset)) fieldTy
  | A.SubscriptVar (arrVar, exp, pos) ->
      let { exp = arrExp; ty = arrTy } = transVar venv tenv senv level arrVar in
      let resultTy =
        match arrTy with
        | T.ARRAY (ty, _) -> actualTy ty pos
        | _ -> error pos "Cannot take subscript of a non-array type."
      in
      let ({ exp = expExp; ty = expTy } as expExpTy) =
        transExp venv tenv senv level exp
      in
      let expTyStr = tyStr expTy in
      let _ =
        if not (checkInt (expExpTy, pos)) then
          error pos
            ("Subscript of array should have type int, not " ^ expTyStr ^ ".")
      in
      return (Translate.subscriptVar (arrExp, expExp)) resultTy

(*  venv * tenv * Absyn.dec -> {venv: venv; tenv: tenv}  *)
and transDec venv tenv senv level dec =
  match dec with
  | A.VarDec { name; escape; typ; init; pos } ->
      let { exp = varExp; ty = initTy; _ } =
        transExp venv tenv senv level init
      in
      let initTyStr = tyStr initTy in
      let _ =
        match typ with
        | Some (sym, pos) ->
            let declTy = getActualType tenv sym pos in
            let declTyStr = tyStr declTy in
            if not (checkTy declTy initTy) then
              error pos
                ("Declared type for "
                ^ quote (S.name name)
                ^ " (" ^ declTyStr ^ ") differs from the initializing type ("
                ^ initTyStr ^ ").")
        | None ->
            if initTy = T.NIL then
              error pos
                "Cannot initialize variable to nil without an explicit type \
                 declaration."
      in
      let access = Tr.allocLocal level !escape in
      ( {
          venv =
            S.enter venv name
              (Env.VarEntry { access; ty = initTy; const = false });
          tenv;
          senv;
        },
        [ varExp ] )
  | A.FunctionDec lst ->
      (* create new value environment *)
      let getFieldType ({ typ; pos; _ } : A.field) =
        getActualType tenv typ pos
      in
      let enterFunctionNames (seen, venv)
          ({ name; params; result; pos } : A.fundec) =
        if contains seen name then
          let nameStr = quote (S.name name) in
          error pos
            ("Function " ^ nameStr
           ^ " was defined multiple times in a sequence of adjacent function \
              declarations.")
        else
          let formalTys = List.map getFieldType params in
          let formalEscapes =
            List.map (fun ({ escape; _ } : A.field) -> !escape) params
          in
          let resultTy =
            match result with
            | Some (sym, pos) -> getActualType tenv sym pos
            | None -> T.UNIT
          in
          let newLevel =
            Tr.newLevel { parent = level; name; formals = formalEscapes }
          in
          ( name :: seen,
            S.enter venv name
              (Env.FunEntry
                 {
                   level = newLevel;
                   label = name;
                   formals = formalTys;
                   result = resultTy;
                 }) )
      in
      let _, venv' = List.fold_left enterFunctionNames ([], venv) lst in
      (* then, typecheck each individual function *)
      let typecheckFunc ({ name; params; body; pos; _ } : A.fundec) =
        let funcName = quote (S.name name) in
        let resultTy =
          match getValue venv' name pos with
          | Env.FunEntry { result; _ } -> result
          | _ ->
              ErrorMsg.impossible
                (funcName
               ^ " is not a function entry while typechecking function bodies."
                )
        in
        let enterParams venv ({ name; escape; typ; pos } : A.field) =
          let paramTy = getActualType tenv typ pos in
          let access = Tr.allocLocal level !escape in
          S.enter venv name
            (Env.VarEntry { access; ty = paramTy; const = true })
        in
        (* create value environment inside function *)
        let venv'' = List.fold_left enterParams venv' params in
        let { exp = bodyExp; ty = bodyTy } =
          transExp venv'' tenv senv level body
        in
        if not (checkTy resultTy bodyTy) then
          let resultTyStr = tyStr resultTy in
          let bodyTyStr = tyStr bodyTy in
          error pos
            ("Function " ^ funcName ^ " should have result type " ^ resultTyStr
           ^ ", but its body has type " ^ bodyTyStr ^ ".")
        else bodyExp
      in
      let newBodyExps = List.map typecheckFunc lst in
      ({ venv = venv'; tenv; senv }, newBodyExps)
  | A.TypeDec lst ->
      (* create new type environment with name types *)
      let enterTypeNames (seen, tenv) ({ name; pos; _ } : A.typedec) =
        if contains seen name then
          let nameStr = S.name name in
          let _ =
            error pos
              ("Type " ^ nameStr
             ^ " was defined multiple times in a sequence of adjacent type \
                declarations.")
          in
          raise SemanticError
        else (name :: seen, S.enter tenv name (T.NAME (name, ref None)))
      in
      let _, tenv' = List.fold_left enterTypeNames ([], tenv) lst in
      let resolveTypes ({ name; ty; pos } : A.typedec) =
        let nameStr = S.name name in
        let resolved = transTy tenv' ty in
        let reference =
          match getType tenv' name pos with
          | T.NAME (_, reference) -> reference
          | _ ->
              ErrorMsg.impossible
                (nameStr
               ^ " is not a NAME entry while typechecking type declarations.")
        in
        reference := Some resolved
      in
      (* resolve each type *)
      List.iter resolveTypes lst;
      (* detect cycles *)
      let detectCycle ({ name; pos; _ } : A.typedec) =
        let ty = getType tenv' name pos in
        let rec detect seen ty =
          match ty with
          | T.NAME (sym, nextTy) ->
              if contains seen sym then
                error pos
                  ("Cycle detected in type declarations for type "
                   ^ S.name sym ^ ".")
              else
                let unboxedTy =
                  match !nextTy with
                  | Some x -> x
                  | None ->
                      ErrorMsg.impossible
                        ("Type " ^ S.name sym ^ " was never resolved.")
                in
                detect (sym :: seen) unboxedTy
          | _ -> ()
        in
        detect [] ty
      in
      let _ = List.map detectCycle lst in
      ({ venv; tenv = tenv'; senv }, [])

(*  tenv * Absyn.ty -> Types.ty  *)
and transTy tenv ty =
  match ty with
  | A.NameTy (sym, pos) -> getType tenv sym pos
  | A.RecordTy lst ->
      let combine ({ name; typ = fieldTySym; pos; _ } : A.field) fields =
        let fieldTy = getType tenv fieldTySym pos in
        (name, fieldTy) :: fields
      in
      let fieldList = List.fold_right combine lst [] in
      T.RECORD (fieldList, ref ())
  | A.ArrayTy (sym, pos) ->
      let ty = getType tenv sym pos in
      T.ARRAY (ty, ref ())

let transProg ast =
  Translate.init ();
  let { exp; ty } =
    transExp Env.base_venv Env.base_tenv base_senv Tr.outermost ast
  in
  (exp, Translate.getResult ())
