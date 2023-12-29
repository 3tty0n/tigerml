open Frame

module A = Absyn
module T = Tree

type exp = Ex of T.exp
         | Nx of T.stm
         | Cx of (Temp.label * Temp.label -> Tree.stm)
[@@deriving show]

type level = { parent : level option; frame : Frame.frame; unique : unit ref; }
[@@deriving show]

type access = level * Frame.access
[@@deriving show]

type newLevelParams = { parent : level; name : Temp.label; formals : bool list }

let outermost =
  let frame = Frame.newFrame { name=Temp.newlabel (); formals=[] } in
  { parent=None; frame=frame; unique=ref () }

let newLevel { parent; name; formals } =
  let frame = Frame.newFrame { name=name; formals=true::formals } in
  { parent=Some parent; frame=frame; unique=ref () }

let formals level =
  let {frame; _} = level in
  List.map (fun x -> (level, x)) (Frame.formals frame)

let static_link l = List.hd (Frame.formals l.frame)

let levelEq a b =
  let {unique=aunique; _} = a in
  let {unique=bunique; _} = b in
  aunique == bunique

let allocLocal ({frame; _} as level) escapes =
  (level, Frame.allocLocal frame escapes)

let default_exp = Ex (Tree.CONST 0)

type frag = Frame.frag

let frags : frag list ref = ref []

let getResult () = !frags

let addFrag frag =
  frags := !frags @ [frag]

let init () =
  frags := []

let rec seq = function
  | [ a ] -> a
  | a :: rst -> Tree.SEQ (a, seq rst)
  | [] -> ErrorMsg.impossible "Empty sequence should nobe passed to this function"

let unEx e =
  let open Tree in
  match e with
  | Ex e -> e
  | Cx genstm ->
    let r = Temp.newtemp () in
    let t = Temp.newlabel() in
    let f = Temp.newlabel ()
    in ESEQ (seq [MOVE (TEMP r, CONST 1);
                  genstm (t, f);
                  LABEL f;
                  MOVE (TEMP r, CONST 0);
                  LABEL t],
             TEMP r)
  | Nx s -> ESEQ (s, CONST 0)

let unNx = function
  | Ex e -> T.EXP e
  | Cx _ as c -> T.EXP (unEx c)
  | Nx s -> s

let unCx = function
  | Cx genstm -> genstm
  | Nx _ ->
    ErrorMsg.impossible "unCx(Nx _) should neber need to be translated"
  | Ex (T.CONST 0) -> fun (_, f) -> T.JUMP (T.NAME f, [ f ])
  | Ex (T.CONST 1) -> fun (t, _) -> T.JUMP (T.NAME t, [ t ])
  | Ex e -> fun (t, f) -> T.CJUMP (T.NE, e, T.CONST 0, t, f)

let simpleVar ((original_level, a), l) =
  let rec do_one_level curr_level frame_addr =
    (* If we are at the right level, then just load from this frame *)
    if original_level.unique = curr_level.unique then Frame.exp a frame_addr
    else
      match curr_level.parent with
      | Some parent ->
        (* Otherwise, follow the static link to the parent *)
        do_one_level parent (Frame.exp (static_link curr_level) frame_addr)
      | None -> ErrorMsg.impossible "Missing parent level"
  in
  Ex (do_one_level l (T.TEMP Frame.fp))

let subscriptVar (var_exp, index_exp) =
  Ex (T.MEM
        (T.BINOP (
            T.PLUS, unEx var_exp,
            T.BINOP (T.MUL, T.CONST (Frame.wordsize), unEx index_exp))))

let stringExp s =
  let lab = Temp.newlabel () in
  frags := Frame.STRING (lab, s) :: !frags;
  Ex (T.NAME lab)

let assignExp (var, exp) = Nx (T.MOVE (unEx var, unEx exp))

let arithmeticOperation (left, op, right) =
  let op' = match op with
    | A.PlusOp -> T.PLUS
    | A.MinusOp -> T.MINUS
    | A.TimesOp -> T.MUL
    | A.DivideOp -> T.DIV
    | _ -> ErrorMsg.impossible "Invalid arithmetic operator"
  in
  Ex (T.BINOP (op', unEx left, unEx right))

let comparisonOperation (left, op, right) =
  let op' = match op with
    | A.EqOp -> T.EQ
    | A.NeqOp -> T.NE
    | A.LtOp -> T.LT
    | A.LeOp -> T.LE
    | A.GtOp -> T.GT
    | A.GeOp -> T.GE
    | _ -> ErrorMsg.impossible "Invalid comparison operator"
  in
  Cx (fun (t, f) -> T.CJUMP (op', unEx left, unEx right, t, f))

let ifThenElse (test, t, f) =
  let t_label = Temp.newlabel () in
  let f_label = Temp.newlabel () in
  let end_label = Temp.newlabel () in
  match t with
  | Nx t' ->
    Nx (
      seq [
        (unCx test) (t_label, f_label);
        T.LABEL t_label;
        t';
        T.JUMP (T.NAME end_label, [ end_label ]);
        T.LABEL f_label;
        unNx f;
        T.LABEL end_label;
      ])
  | _ ->
    let r = Temp.newtemp () in
    Ex (
      T.ESEQ (
        seq [
          (unCx test) (t_label, f_label);
          T.LABEL t_label;
          T.MOVE (T.TEMP r, unEx t);
          T.JUMP (T.NAME end_label, [ end_label ]);
          T.LABEL f_label;
          T.MOVE (T.TEMP r, unEx f);
          T.LABEL end_label;
        ],
        T.TEMP r
      ))

let ifThen (test, t) =
  let t_label = Temp.newlabel () in
  let end_label = Temp.newlabel () in
  Nx (
    seq [
      (unCx test) (t_label, end_label);
      T.LABEL t_label;
      unNx t;
      T.LABEL end_label;
    ]
  )

let arrayExp (size, init) =
  Ex (Frame.externalCall ("initArray", [ unEx size; unEx init ]))

let recordExp fields =
  let num_fields = List.length fields in
  let record_size = num_fields * Frame.wordsize in
  let r = Temp.newtemp () in
  let _, processed_fields = List.fold_left (fun (idx, fs) field ->
      ( idx + 1,
        T.MOVE (
          T.MEM (
            T.BINOP (T.PLUS, (T.TEMP r), T.CONST (idx * Frame.wordsize))
          ),
          unEx field
        ) :: fs
      )
    ) (0, []) fields in
  Ex
    (T.ESEQ (
        seq (
          [
            T.MOVE (
              T.TEMP r,
              Frame.externalCall ("malloc", [ T.CONST record_size ]))
          ] @ processed_fields
        ),
        T.TEMP r
      ))

let whileExp (test, body, end_label) =
  let start_label = Temp.newlabel () in
  let body_label = Temp.newlabel () in
  Nx (
    seq (
      [
        T.LABEL start_label;
        (unCx test) (body_label, end_label);
        T.LABEL body_label;
        unNx body;
        T.JUMP (T.NAME start_label, [ start_label ]);
        T.LABEL end_label;
      ]
    )
  )

let breakExp label = Nx (T.JUMP (T.NAME label, [ label ]))

let forExp (counter, lo, hi, body, end_label) =
  let r = Temp.newtemp () in
  let start_label = Temp.newlabel () in
  let increment_label = Temp.newlabel () in
  Nx (
    (seq
       [
         (unNx (assignExp (counter, lo)));
         T.MOVE (T.TEMP r, unEx hi);
         T.CJUMP (T.LE, unEx counter, T.TEMP r, start_label, end_label);
         T.LABEL start_label;
         unNx body;
         T.CJUMP (T.LT, unEx counter, T.TEMP r, increment_label, end_label);
         T.LABEL increment_label;
         unNx
           (assignExp (counter, Ex (T.BINOP (T.PLUS, unEx counter, T.CONST 1))));
         T.JUMP (T.NAME start_label, [ start_label ]);
         T.LABEL end_label;
       ])
  )

let computeDeclaringFrameAddress (decLevel, level) =
  let rec findFrame currLevel currAddr =
    let {parent; frame=currFrame; _} = currLevel in
    if levelEq decLevel currLevel then
      (* we are in the correct frame *)
      currAddr
    else
      (* the frame we want is further up the chain *)
      let slAccess = match Frame.formals currFrame with
        hd :: _ -> hd
      | [] -> ErrorMsg.impossible "found a frame with no formals - there should always be at least one formal for the static link"
      in
      let parentAddr = Frame.exp slAccess currAddr in
      let parentLevel = match parent with
        Some level -> level
      | None -> ErrorMsg.impossible "followed static links to the outermost level. this should never happen"
      in
      findFrame parentLevel parentAddr
  in
  findFrame level (T.TEMP Frame.fp)

let callExp (label, fun_level, call_level, args) =
  let { parent; _ } : level = fun_level in
  let unExArgs = List.map unEx args in
  let parent_level = match parent with
    | Some level -> level
    | None -> ErrorMsg.impossible "called a function with no enclosing parent level"
  in
  let call =
    let declaring_frame_addr =  computeDeclaringFrameAddress (parent_level, call_level) in
    T.CALL (T.NAME label, declaring_frame_addr :: unExArgs)
  in
  Ex call

 (* Assume that stdlib functions do not require a static link *)
let callStdlibExp (name, args) =
  Ex (T.CALL (T.NAME name, T.CONST 0 :: List.map unEx args))

let fieldVar (var_exp, field_index) =
  (* Store the record pointer in a register so we can
     check that it is not null *)
  let r = Temp.newtemp () in
  let quit_label = Temp.newlabel () in
  let ok_label = Temp.newlabel () in
  Ex
    (T.ESEQ
       ( seq
           [
             T.MOVE (T.TEMP r, unEx var_exp);
             T.CJUMP (T.EQ, T.TEMP r, T.CONST 0, quit_label, ok_label);
             T.LABEL quit_label;
             (* If we encounter a null pointer, print a message and exit *)
             unNx
               (callStdlibExp
                  ( Temp.namedlabel "print",
                    [ stringExp "Null pointer dereference" ] ));
             unNx
               (callStdlibExp (Temp.namedlabel "exit", [ Ex (T.CONST 1) ]));
             T.LABEL ok_label;
           ],
         T.MEM
           (T.BINOP (
               T.PLUS,
               (T.TEMP r),
               T.BINOP (T.MUL, (T.CONST Frame.wordsize), (T.CONST field_index))
             ))))

let intExp i = Ex (T.CONST i)

let seqExp exps =
  match exps with
  | [ e ] -> e
  | e :: [ r ] -> Ex (T.ESEQ (unNx e, unEx r))
  | _ ->
    let rev = List.rev exps in
    let last = List.hd rev in
    let rest = List.rev (List.tl rev) in
    Ex (T.ESEQ (seq (List.map unNx rest), unEx last))

let procEntryExit (exp, level) =
  let body_with_return = T.MOVE (T.TEMP Frame.rv, unEx exp) in
  let body_with_lable =
    T.SEQ (T.LABEL (Frame.name level.frame), body_with_return)
  in
  let processed_body = Frame.procEntryExit1 (level.frame, body_with_lable) in
  let frag = Frame.PROC { frame = level.frame; body = processed_body } in
  frags := frag :: !frags;
  ()
