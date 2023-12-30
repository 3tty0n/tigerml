open Temp
module T = Tree

let linearize stm0 =
  let ( % ) x y =
    match (x, y) with
    | T.EXP (T.CONST _), _ -> y
    | _, T.EXP (T.CONST _) -> x
    | _, _ -> T.SEQ (x, y)
  in
  let commute = function
    | T.EXP (T.CONST _), _ -> true
    | _, T.NAME _ -> true
    | _, T.CONST _ -> true
    | _ -> false
  in
  let nop = T.EXP (T.CONST 0) in

  let rec reorder = function
    | (T.CALL _ as e) :: rest ->
        let t = Temp.newtemp () in
        reorder (T.ESEQ (T.MOVE (T.TEMP t, e), T.TEMP t) :: rest)
    | a :: rest ->
        let stms, e = do_exp a in
        let stms', el = reorder rest in
        if commute (stms', e) then (stms % stms', e :: el)
        else
          let t = Temp.newtemp () in
          (stms % T.MOVE (T.TEMP t, e) % stms', T.TEMP t :: el)
    | [] -> (nop, [])
  (* Similar to reorder_stm, but it returns a pair (s, e) where s is a
     statement containing all the side effects pulled out of the list of
     expressions, and e is build(l') *)
  and reorder_exp (el, build) =
    let stms, el' = reorder el in
    (stms, build el')
  (* Pulls all the ESEQs out of the list, yielding a statement s' that
      contains all the statements from the ESEQs and a list l' of
      cleaned-up expressions. Then makes SEQ(s',build(l')) *)
  and reorder_stm (el, build) =
    let stms, el' = reorder el in
    stms % build el'
  and do_stm = function
    | T.SEQ (a, b) -> do_stm a % do_stm b
    | T.JUMP (e, labs) ->
        reorder_stm
          ( [ e ],
            function
            | [ e ] -> T.JUMP (e, labs)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.CJUMP (p, a, b, t, f) ->
        reorder_stm
          ( [ a; b ],
            function
            | [ a; b ] -> T.CJUMP (p, a, b, t, f)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.MOVE (T.TEMP t, T.CALL (e, el)) ->
        reorder_stm
          ( e :: el,
            function
            | e :: el -> T.MOVE (T.TEMP t, T.CALL (e, el))
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.MOVE (T.TEMP t, b) ->
        reorder_stm
          ( [ b ],
            function
            | [ b ] -> T.MOVE (T.TEMP t, b)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.MOVE (T.MEM e, b) ->
        reorder_stm
          ( [ e; b ],
            function
            | [ e; b ] -> T.MOVE (T.MEM e, b)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.MOVE (T.ESEQ (s, e), b) -> do_stm (T.SEQ (s, T.MOVE (e, b)))
    | T.EXP (T.CALL (e, el)) ->
        reorder_stm
          ( e :: el,
            function
            | e :: el -> T.EXP (T.CALL (e, el))
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.EXP e ->
        reorder_stm
          ( [ e ],
            function
            | [ e ] -> T.EXP e
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | s ->
        reorder_stm
          ( [],
            function
            | [] -> s | _ -> ErrorMsg.impossible "This should never be possible"
          )
  and do_exp = function
    | T.BINOP (p, a, b) ->
        reorder_exp
          ( [ a; b ],
            function
            | [ a; b ] -> T.BINOP (p, a, b)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.MEM a ->
        reorder_exp
          ( [ a ],
            function
            | [ a ] -> T.MEM a
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | T.ESEQ (s, e) ->
        let stms = do_stm s in
        let stms', e = do_exp e in
        (stms % stms', e)
    | T.CALL (e, el) ->
        reorder_exp
          ( e :: el,
            function
            | e :: el -> T.CALL (e, el)
            | _ -> ErrorMsg.impossible "This should never be possible" )
    | e ->
        reorder_exp
          ( [],
            function
            | [] -> e | _ -> ErrorMsg.impossible "This should never be possible"
          )
  in
  (* linear gets rid of the top-level SEQ's, producing a list *)
  let rec linear = function
    | T.SEQ (a, b), l -> linear (a, linear (b, l))
    | s, l -> s :: l
  in
  linear (do_stm stm0, [])

let basicBlocks stms =
  let done_label = Temp.newlabel () in
  (* Pass in remaining statements and an accumulator list *)
  let rec blocks = function
    | (T.LABEL _ as head) :: tail, blist ->
        let rec next = function
          (* A block is ended when a JUMP/CJUMP is encountered *)
          | (T.JUMP _ as s) :: rest, thisblock -> endblock (rest, s :: thisblock)
          | (T.CJUMP _ as s) :: rest, thisblock ->
              endblock (rest, s :: thisblock)
          (* A new block is started when a LABEL is encountered *)
          | (T.LABEL lab :: _ as stms), thisblock ->
              next (T.JUMP (T.NAME lab, [ lab ]) :: stms, thisblock)
          | s :: rest, thisblock -> next (rest, s :: thisblock)
          (* When we run out of statements, end the block with a
             jump to the done label *)
          | [], thisblock ->
              next ([ T.JUMP (T.NAME done_label, [ done_label ]) ], thisblock)
        and endblock (stms, thisblock) =
          blocks (stms, List.rev thisblock :: blist)
        in
        next (tail, [ head ])
    | [], blist -> List.rev blist
    (* If a block doesn't start with a label, we stick an invented
       label at the beginning *)
    | stms, blist -> blocks (T.LABEL (Temp.newlabel ()) :: stms, blist)
  in
  (blocks (stms, []), done_label)

(* Enters a block to the table, this only works if the block starts with a LABEL.
   The block is added to the table using the said label. *)
let enterblock block table =
  match block with T.LABEL s :: _ as b -> Symbol.enter table s b | _ -> table

(* Returns the original list without its last element, and the last
   element itself. This function does not accept the empty list. *)
let rec splitlast = function
  | [ x ] -> ([], x)
  | h :: t ->
      let t', last = splitlast t in
      (h :: t', last)
  | [] ->
      ErrorMsg.impossible
        "It shouldn't be possible for an empty list to be passed to splitlast"

let rec trace (table, block, rest) =
  match block with
  | T.LABEL lab :: _ as b -> (
      (* Mark a block as processed by changing it to the empty list *)
      let table = Symbol.enter table lab [] in
      match splitlast b with
      | most, T.JUMP (T.NAME lab, _) -> (
          (* We try to skip the jump instruction by falling through
             to the destination block, i.e. we make the jump unconditional *)
          match Symbol.look table lab with
          | Some (_ :: _ as b') -> most @ trace (table, b', rest)
          (* If the false block is already marked, then we are unable to skip
             the jump, hence we just move on with the rest. *)
          | _ -> b @ getnext (table, rest))
      | most, T.CJUMP (opr, x, y, t, f) -> (
          match (Symbol.look table t, Symbol.look table f) with
          (* We try to skip the jump instruction by falling through
             to the false block *)
          | _, Some (_ :: _ as b') -> b @ trace (table, b', rest)
          (* If we only find the true block, we negate the condition
             so that we can fall through to the true block instead (so
             that we consistently fall through to the 'false' block ) *)
          | Some (_ :: _ as b'), _ ->
              most
              @ [ T.CJUMP (T.not_rel opr, x, y, f, t) ]
              @ trace (table, b', rest)
          (* We do this to ensure that each CJUMP is followed by its false
             label no matter what *)
          | _ ->
              let f' = Temp.newlabel () in
              most
              @ [
                  T.CJUMP (opr, x, y, t, f');
                  T.LABEL f';
                  T.JUMP (T.NAME f, [ f ]);
                ]
              @ getnext (table, rest))
      (* If we are not jumping to a label we can't optimise anything,
         hence we move on. *)
      | _, T.JUMP _ -> b @ getnext (table, rest)
      | _ ->
          ErrorMsg.impossible
            "Impossible for last statement in the block to not be a JUMP or \
             CJUMP")
  | _ ->
      ErrorMsg.impossible
        "Impossible for block to not start with a label in trace function"

(* Fetches the next block and processes it *)
and getnext = function
  | table, (T.LABEL lab :: _ as b) :: rest -> (
      match Symbol.look table lab with
      | Some (_ :: _) -> trace (table, b, rest)
      (* When the block is empty, it means that we have already processed it,
         i.e. we can move on *)
      | _ -> getnext (table, rest))
  | _, [] -> []
  | _ ->
      ErrorMsg.impossible
        "Impossible for block to not start with a label in getnext function"

let traceSchedule (blocks, done_label) =
  (* TODO: Why fold_right? *)
  getnext (List.fold_right enterblock blocks Symbol.empty, blocks)
  @ [ T.LABEL done_label ]
