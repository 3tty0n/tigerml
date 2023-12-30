module A = Absyn
module S = Symbol

type depth = int
type escEnv = (depth * bool ref) S.table

let rec traverseVar env d s =
  match s with
  | A.SimpleVar (sym, _) -> (
      match S.look env sym with
      | Some (d', escapes) -> if d > d' then escapes := true
      | None ->
          ErrorMsg.impossible
            ("Variable entry not found for " ^ S.name sym
           ^ " while calculating escpaes"))
  | A.FieldVar (var, _, _) -> traverseVar env d var
  | A.SubscriptVar (var, _, _) -> traverseVar env d var

and traverseExp env d s =
  let traverseExps exps = List.iter (traverseExp env d) exps in
  match s with
  | A.VarExp var -> traverseVar env d var
  | A.NilExp -> ()
  | A.IntExp _ -> ()
  | A.StringExp _ -> ()
  | A.CallExp { args; _ } -> traverseExps args
  | A.OpExp { left; right; _ } ->
      traverseExp env d left;
      traverseExp env d right
  | A.RecordExp { fields; _ } ->
      List.iter (fun (_, exp, _) -> traverseExp env d exp) fields
  | A.SeqExp lst -> List.iter (fun (exp, _) -> traverseExp env d exp) lst
  | A.AssignExp { var; exp; _ } ->
      traverseVar env d var;
      traverseExp env d exp
  | A.IfExp { test; then'; else'; _ } -> (
      match else' with
      | Some e -> traverseExps [ test; then'; e ]
      | None -> traverseExps [ test; then' ])
  | A.WhileExp { test; body; _ } -> traverseExps [ test; body ]
  | A.ForExp { var; escape; lo; hi; body; _ } ->
      traverseExps [ lo; hi ];
      let env' = S.enter env var (d, escape) in
      traverseExp env' d body
  | A.BreakExp _ -> ()
  | A.LetExp { decs; body; _ } ->
      let env' = traverseDecs env d decs in
      traverseExp env' d body
  | A.ArrayExp { init; _ } -> traverseExp env d init

and traverseDecs env d s =
  let traverseFundec env ({ params; body } : A.fundec) =
    let new_depth = d + 1 in
    let add_param env ({ name; escape = r; _ } : A.field) =
      r := false;
      S.enter env name (new_depth, r)
    in
    let env' = List.fold_left add_param env params in
    traverseExp env' new_depth body
  in
  let traverseDec env = function
    | A.VarDec { name; escape = r; init; _ } ->
      traverseExp env d init;
      r := false;
      S.enter env name (d, r);
    | A.FunctionDec fundecs ->
        let _ = List.iter (traverseFundec env) fundecs in
        env
    | _ -> env
  in
  List.fold_left traverseDec env s

let findEscape (prog : Absyn.exp) =
  let table = S.empty in
  traverseExp table 0 prog
