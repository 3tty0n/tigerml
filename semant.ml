module Translate = struct
  type exp = unit
  let dummy_fixme = ()
end

module A = Absyn
module S = Symbol
module T = Types
module E = Env

type venv = E.enventry S.table
type tenv = T.ty S.table

type expty = { exp : Translate.exp; ty : Types.ty }

type transdecty = { venv : venv; tenv : tenv }

let get_typ = function Some ty -> ty | None -> T.BOTTOM

let rec actual_ty ty ~pos =
  match ty with
  | Types.NAME (name, typ_opt_ref) ->
    (match !typ_opt_ref with
     | None -> failwith ("unkown type:" ^ (S.name name))
     | Some ty -> actual_ty ty ~pos)
  | Types.UNIT
  | Types.NIL
  | Types.INT
  | Types.STRING
  | Types.RECORD _
  | Types.ARRAY _
  | Types.BOTTOM -> ty

let get_typ_from_env ~sym ~env ~pos =
  match Symbol.look env sym with
  | Some ty -> ty
  | None -> failwith ("Unknown type:" ^ S.name sym)

let get_typ_actual_from_env ~sym ~env ~pos =
  actual_ty (get_typ_from_env ~sym ~env ~pos) ~pos

let check_type_assignable var value pos err_msg =
  if T.comp var value = T.EQ || T.comp var value = T.GT
  then ()
  else failwith err_msg

let nest_depth = ref 0
let set_nest_depth n = nest_depth := n

let rec trans_exp venv tenv exp =
  let open Absyn in
  let rec trexp exp =
    match exp with
    | VarExp var -> trvar var
    | NilExp -> return_nil ()
    | IntExp _ -> return_int ()
    | StringExp {string=_; _} -> return_string ()
    | OpExp { left; oper; right; pos } ->
      let expty_left = trexp left in
      let expty_right = trexp right in
      check_same expty_left expty_right pos;
      (match oper with
       | PlusOp | MinusOp | TimesOp | DivideOp ->
         check_int expty_left pos;
         check_int expty_right pos;
         return_int ()
       | EqOp | NeqOp ->
         let { exp=_; ty } = expty_left in
         if T.is_int ty || T.is_string ty then
           return_int ()
         else
           failwith "invalid operation types."
       | LtOp | LeOp | GtOp | GeOp ->
         let { exp=_; ty } = expty_left in
         if T.is_int ty || T.is_string ty then
           return_int ()
         else
           failwith "invalid operation types."
      )
    | SeqExp [] ->
      return_unit ()
    | SeqExp exps ->
      exps
      |> List.map (fun (exp, _) -> trexp exp)
      |> List.rev
      |> List.hd
    | RecordExp { fields; typ; pos } ->
      begin
        match Sym.look tenv typ with
        | Some x ->
          begin
            match x with
            | T.RECORD (f, _) ->
              let rec_formal : (S.t * S.t) list = f () in
              let rec get_field_typename name = function
                  [] -> Types.BOTTOM
                | (sym, exp, pos) :: l ->
                  if String.compare name (S.name sym) = 0
                  then (trexp exp).ty
                  else get_field_typename name l
              in
              let check_formal sym ty =
                if not (Types.leq (get_field_typename (S.name sym) fields) ty)
                then failwith ("actual type doesn't mach formal type: " ^ (S.name sym))
                else ()
              in
              let iterator (fieldname, typeid) =
                match S.look tenv typeid with
                | Some x -> check_formal fieldname x; ()
                | None -> failwith ("unknown type in record: " ^ S.name typ)
              in
              if List.length rec_formal <> List.length fields
              then failwith ("record list is wrong length: " ^ S.name typ)
              else
                rec_formal |> List.iter (fun (fieldname, typeid) ->
                  iterator (fieldname, typeid)
                ); return_unit ()
            | _ -> failwith ("expected record type, not :" ^ Sym.name typ)
          end
        | None -> failwith ("expected record type, not :" ^ Sym.name typ)
      end
    | AssignExp { var; exp; pos } ->
      let rec get_var_symbol = function
        | SimpleVar { symbol; _ } -> S.look venv symbol
        | FieldVar { var; _; } -> get_var_symbol var
        | SubScriptVar { var; _; } -> get_var_symbol var
      in
      let can_assign var' =
        match get_var_symbol var' with
        | Some (Env.VarEntry { ty; read_only }) ->
          if read_only
          then failwith "error: index variable erroneously assigned to"
          else ()
        | _ -> failwith "cannot assign to a function"
      in
      can_assign var;
      return_unit ()
    | CallExp { func; args; pos } ->
      (match S.look venv func with
       | Some (Env.VarEntry { ty; read_only }) ->
         failwith @@ Printf.sprintf "expected funciton, but found variable %s" (S.name func)
       | Some (Env.FunEntry { formals; result }) ->
         List.iter2 (fun t_arg ty_formal ->
             let { exp; ty } = trexp t_arg in
             if ty <> ty_formal
             then failwith @@ Printf.sprintf "Unmatched type: %s %s" (T.show_ty ty) (T.show_ty ty_formal)
           ) args formals;
         return_unit ()
       | None -> failwith @@ "Cannot find function " ^ (S.name func)
      )
    | IfExp { test; then'; else'; pos } ->
      check_int (trexp test) pos;
      let { exp=_; ty=ty_then } = trexp then' in
      (match else' with
       | None -> return ty_then
       | Some else' ->
         let {exp=_; ty=ty_else } = trexp else' in
         if ty_then <> ty_else then failwith "the types of then and else are not same"
         else return ty_then)
    | WhileExp { test; body; pos } ->
      check_int (trexp test) pos;
      check_unit (trexp body) pos;
      return_unit ()
    | ForExp { var; escape; lo; hi; body; pos } ->
      check_int (trexp lo) pos;
      check_int (trexp hi) pos;
      check_unit (trexp body) pos;
      return_unit ()
    | BreakExp _ -> return_unit ()
    | ArrayExp { typ; size; init; pos } ->
      (match S.look tenv typ with
       | Some x ->
         (match actual_ty x ~pos with
          | T.ARRAY (ty, unique) ->
            check_int (trexp size) pos;
            check_same_ty (trexp init).ty (actual_ty ty ~pos) "Not same type in array createion";
            return (T.ARRAY (ty, unique))
          | _ -> failwith "Not of ARRAY type in array creaation")
       | None -> failwith "No such type")
    | LetExp { decs; body; pos } ->
      let cur_depth = !nest_depth in
      set_nest_depth 0;
      let {venv=venv'; tenv=tenv'} = trans_dec venv tenv decs in
      set_nest_depth cur_depth;
      trans_exp venv' tenv' body
  and trvar var =
    match var with
    | SimpleVar { symbol; pos } ->
      (match S.look venv symbol with
       | Some (Env.VarEntry { ty; read_only }) -> return ty
       | Some (Env.FunEntry { formals; result }) -> return result
       | None -> failwith @@ "error: undeclared variable " ^ (S.name symbol))
    | FieldVar { var; symbol; pos } ->
      (match trvar var with
        | { exp=(); ty=T.RECORD (recgen, unique) } ->
          let fields = recgen () in
          let rec get_field_type fields id pos =
            match fields with
            | (fsym, fty) :: l ->
              if String.compare (S.name fsym) (S.name id) = 0
              then
                match S.look tenv fty with
                | Some ty -> ty
                | None -> failwith "type error in record"
              else get_field_type l id pos
            | [] -> failwith "no such field"
          in return (get_field_type fields symbol pos)
        | { exp; ty; } -> failwith "error : variable not record")
    | SubScriptVar { var; exp; pos } ->
      (match trvar var with
       | {exp=(); ty=T.ARRAY (arr_ty, unique)} ->
         check_int (trexp exp) pos; return (actual_ty arr_ty ~pos)
       | {exp; ty} -> failwith "requires array"
      )
  in trexp exp

and check_int { exp; ty } pos =
  match ty with
  | Types.INT -> ()
  | _ -> failwith "integer required."

and check_unit { exp; ty } pos =
  match ty with
  | T.UNIT -> ()
  | _ -> failwith "unit required."

and check_same { exp=exp1; ty=ty1 } { exp=exp2; ty=ty2 } pos =
  if ty1 == ty2 then () else failwith "same types are required."

and check_same_ty ty1 ty2 msg =
  if T.leq ty1 ty2 then () else failwith msg

and return_unit () = { exp=(); ty=Types.UNIT }

and return_nil () = { exp=(); ty=Types.NIL }

and return_int () = { exp=(); ty=Types.INT }

and return_string () = { exp=(); ty=Types.STRING }

and return (ty : T.ty) = { exp=(); ty=ty }

and trans_dec venv tenv decs : transdecty =
  let rec trdec {venv; tenv} dec =
    match dec with
    | A.VarDec { name; escape; typ; init; pos } ->
      let get_types = function
          Some ty -> ty
        | None -> T.BOTTOM
      in
      let rec actual_ty ty =
        match ty with
        | T.NAME (symbol, ty) ->
          actual_ty (get_types (S.look tenv name))
        | some_ty -> some_ty
      in
      (match typ with
       | Some (symbol, pos) ->
         begin
           match S.look tenv symbol with
           | Some ty ->
             check_type_assignable (actual_ty ty) ty pos "erro : mismatched types in vardec";
             { venv=S.enter venv name (Env.VarEntry { ty=actual_ty ty; read_only=false})
             ; tenv=tenv
             }
           | None ->
             failwith "type not recognized"
         end
       | None ->
         let { exp; ty } = trans_exp venv tenv init in
         (if T.leq ty T.NIL
          then failwith "error: initializing nil expressions not constrained by record type"
          else ());
         { venv=S.enter venv name (Env.VarEntry { ty=ty; read_only=false })
         ; tenv=tenv
         })
    | A.TypeDec tydecs ->
      (* let make_temp_tydec tenv A.{name; ty; pos} = S.enter tenv name T.BOTTOM in *)
      (* let temp_tenv = List.fold_left make_temp_tydec tenv tydecs in *)
      let fold_tydec {venv; tenv} A.{name; ty; pos} = { venv=venv; tenv=S.enter tenv name (trans_ty tenv ty) } in
      let new_env = List.fold_left fold_tydec {venv; tenv} tydecs in

      let check_illegal_cycle () A.{name; ty; pos} =
        let rec check_helper seen name =
          match S.look new_env.tenv name with
          | Some (T.NAME (sym, _)) ->
            if List.exists (fun y -> String.compare (S.name sym) (S.name y) = 0) seen
            then failwith "error: mutally recursive types that do not pass through record or array"
            else check_helper (name :: seen) name
          | _ -> ()
        in check_helper [] name
      in

      let check_duplicates seen A.{name; ty; pos} =
        if List.exists (fun y -> String.compare (S.name name) y = 0) seen
        then failwith "error: two types of same name in mutually recursive tydec"
        else (S.name name) :: seen
      in

      List.fold_left check_duplicates [] tydecs |> ignore;
      List.fold_left check_illegal_cycle () tydecs;
      new_env
    | A.FunctionDec fundecs ->
      let trans_rt rt =
        match S.look tenv rt with
        | Some rt -> rt
        | None -> failwith @@ "return type unrecognized " ^ (S.name rt)
      in
      let trans_param (A.Field { name; escape; typ; pos }) =
        match S.look tenv typ with
        | Some t -> (name, t)
        | None -> failwith @@ "Parameter type unrecognized " ^ (S.name typ)
      in
      let enter_funcs A.{ name; params; result; body; pos } venv =
        match result with
        | Some (rt, pos') ->
          S.enter venv name
            E.(FunEntry {formals=List.map (fun param -> snd (trans_param param)) params; result=trans_rt rt})
        | None ->
          S.enter venv name
            E.(FunEntry {formals=List.map (fun param -> snd (trans_param param)) params; result=T.UNIT})
      in
      let venv' = List.fold_right enter_funcs fundecs venv in
      let check_fundec A.{ name; params; body; pos; result } =
        let result_ty = match result with Some (rt, pos') -> trans_rt rt | None -> T.UNIT in
        let params' = List.map trans_param params in
        let enter_param venv (name, ty) = S.enter venv name (E.VarEntry {ty=ty; read_only=false}) in
        let venv'' = List.fold_left enter_param venv' params' in
        let body' = trans_exp venv'' tenv body in
        Printf.eprintf "body type : %s, result type : %s\n " (body'.ty |> T.show_ty) (result_ty |> T.show_ty);
        if not (T.leq body'.ty result_ty)
        then failwith @@ "Function body type doesn't match return type in function " ^ (S.name name)
      in
      let fold_fundec fundec _ = check_fundec fundec in
      let check_duplicates seen A.{name; _} =
        if List.exists (fun y -> String.compare (S.name name) y = 0) seen then
          failwith @@ "error : two types of same name in mutually recursive fundec"
        else
          (S.name name) :: seen
      in
      List.fold_left check_duplicates [] fundecs |> ignore;
      List.fold_right fold_fundec fundecs ();
      {venv=venv'; tenv=tenv}
  and fold_dec {venv; tenv} dec = trdec {venv; tenv} dec in
  List.fold_left fold_dec {venv=venv; tenv=tenv} decs

(* tenv -> Absyn.ty -> Types.ty *)
and trans_ty tenv ty =
  let rec trty tenv = function
    | A.NameTy { symbol; _ } ->
      (match S.look tenv symbol with
       | Some _ -> T.NAME (symbol, ref None)
       | None ->  failwith @@ "unrecognized name type: " ^ S.name symbol
      )
    | A.RecordTy fields ->
      let field_process (A.Field { name; escape; typ; pos }) =
        match S.look tenv name with
        | Some _ -> (name, typ)
        | None -> failwith @@ "undefined type in rec: " ^ S.name typ
      in
      let rec rec_gen () = List.fold_left (fun acc field -> field_process field :: acc) [] fields in
      T.RECORD (rec_gen, ref ())
    | A.ArrayTy { symbol; pos } ->
      T.ARRAY (trans_ty tenv (A.NameTy { symbol; pos }), ref ())
  in trty tenv ty

let trans_prog : A.exp -> unit = fun exp ->
  let {exp=_; ty=_} = trans_exp E.base_venc E.base_tenv exp in
  ()
