open Frame
module A = Assem
module T = Tree

module type CODEGEN = sig
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

module RiscVGen : CODEGEN = struct
  let codegen frame stm =
    let ir_list : A.instr list ref = ref [] in
    let emit instr = ir_list := instr :: !ir_list in

    let result gen =
      let t = Temp.newtemp () in
      gen t;
      t
    in

    let calldefs = [ Frame.rv; Frame.rv ] @ Frame.caller_save_regs in

    let rec munch_stm = function
      | T.SEQ (a, b) ->
          munch_stm a;
          munch_stm b
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)\n" i;
                 src = [ munch_exp e1; munch_exp e2; Temp.newtemp () ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)\n" i;
                 src = [ munch_exp e1; munch_exp e2; Temp.newtemp () ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, T.MEM e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tld 's1, 's0\n\tsd 's1, 0('s1)\n";
                 src = [ munch_exp e1; munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM (T.CONST i), e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tli 's0, %d\n\tsd 's1, 0('s0)\n" i;
                 src = [ Temp.newtemp (); munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tsd 's1, 0('s0)\n";
                 src = [ munch_exp e1; munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.TEMP i, e2) ->
          emit
            (A.MOVE
               {
                 assem = Printf.sprintf "\tmv 'd0, 's0\n";
                 src = munch_exp e2;
                 dst = i;
               })
      | T.MOVE (_, _) as stm ->
        Printf.eprintf "Invalid MOVE: %s\n" (T.show_stm stm);
        ErrorMsg.impossible "MOVE's first operand should be TEMP or MEM"
      | T.LABEL lab ->
          emit
            (A.LABEL
               {
                 assem = Printf.sprintf "%s:\n" (Temp.string_of_label lab);
                 lab;
               })
      | T.JUMP (e, labels) -> (
          match e with
          | T.NAME label ->
              emit
                (A.OPER
                   {
                     assem = "\tj 'j0\n";
                     src = [];
                     dst = [];
                     jump = Some [ label ];
                   })
          | _ -> ErrorMsg.impossible "Impossible to jump to a non-label.")
      | T.EXP exp ->
        (match exp with
        | T.BINOP (_, e1, e2) ->
          ignore (munch_exp e1);
          ignore (munch_exp e2);
        | T.CALL (e, args) ->
          (match e with
          | T.NAME name ->
            emit
              (A.OPER
                 {
                   assem =
                     Printf.sprintf "\tcall %s\n" (Temp.string_of_label name);
                   (* munch_args returns list of temporaries that are
                      used in the function call, we put them here so that
                      future liveliness analysis can tell that these values
                      are needed up until the call *)
                   src = munch_args (0, args);
                   dst = calldefs;
                   jump = None;
                 })
          | _ -> ErrorMsg.impossible "function call should be applied to label")
        | T.ESEQ (stm, e) ->
          munch_stm stm;
          ignore (munch_exp e)
        | _ -> ())
      | T.CJUMP (op, e1, e2, t_label, f_label) ->
        let op' = match op with
          | T.EQ -> "beq"
          | T.NE -> "bne"
          | T.LT | T.LE -> "blt"
          | T.GT | T.GE -> "bgt"
        in
        let e1_tmp = munch_exp e1 in
        let e2_tmp = munch_exp e2 in
        if op = T.LE || op = T.GE then
          emit (
            A.OPER {
              assem = "\taddi, 's0, 's0, 1\n";
              src = [ e2_tmp ];
              dst = [];
              jump = None
            }
          );
        emit (
          A.OPER {
            assem = Printf.sprintf "\t%s 's0, 's1, 'j0\n" op';
            src = [ e1_tmp; e2_tmp ];
            dst = [];
            jump = Some [ t_label ]
          }
        )

    and munch_exp = function
      | T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem =
                       Printf.sprintf "\taddi 'd0, 's0, %d\n\tld 'd0, 0('d0)\n"
                         i;
                     src = [ munch_exp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem =
                       Printf.sprintf "\taddi 'd0, 's0, %d\n\tld 'd0, 0('d0)\n"
                         i;
                     src = [ munch_exp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.MEM (T.CONST i) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tli 'd0, %d\n\tld 'd0, 0('d0)\n" i;
                     src = [];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.MEM e1 ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tld 'd0, 0('s0)\n";
                     src = [ munch_exp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (T.PLUS, e1, T.CONST i) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\taddi 'd0, 's0, %d\n" i;
                     src = [ munch_exp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (T.PLUS, T.CONST i, e1) ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\taddi 'd0, 's0, %d\n" i;
                     src = [ munch_exp e1 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.CONST i ->
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\tli 'd0, %d\n" i;
                     src = [];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.BINOP (op, e1, e2) ->
          let map_op = function
            | T.PLUS -> "add"
            | T.MINUS -> "sub"
            | T.MUL -> "mul"
            | T.DIV -> "div"
            | T.AND -> "and"
            | T.OR -> "or"
            | T.XOR -> "xor"
            | T.LSHIFT -> "sll"
            | T.RSHIFT -> "srl"
          in
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\t%s 'd0, 's0, 's1\n" (map_op op);
                     src = [ munch_exp e1; munch_exp e2 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.TEMP t -> t
      | T.NAME lab ->
        result (fun r ->
            emit
              (A.OPER
                 {
                   assem = Printf.sprintf "%s:\n" (Temp.string_of_label lab);
                   src = [];
                   dst = [ r ];
                   jump = None
                 }))
      | T.CALL (e, args) ->
        result (fun r ->
            match e with
            | T.NAME lab ->
              emit (
                A.OPER {
                  assem = Printf.sprintf "\tcall %s\n\tmv 'd0, 'd1\n" (Temp.string_of_label lab);
                  src = munch_args (0, args);
                  dst = r :: Frame.rv :: calldefs;
                  jump = None
                }
              )
            | _ ->
              ErrorMsg.impossible "function must be called with label."
          )
      | T.ESEQ (stm, e) ->
        munch_stm stm; munch_exp e

    and munch_args (i, args) =
      match args with
      | arg :: rst ->
        (match List.nth_opt Frame.arg_regs i with
         | Some reg ->
           emit (
             A.MOVE
               { assem = "\tmv 'd0, 's0\n";
                 src = munch_exp arg;
                 dst = reg;
               });
           reg :: munch_args (i + 1, rst)
         | None ->
           let offset = Frame.wordsize * (i - List.length Frame.arg_regs) in
           (emit
              (A.OPER
                 { assem = Printf.sprintf "\tsd 's0, %d('d0)\n" offset;
                   src = [ munch_exp arg ];
                   dst = [ Frame.fp ];
                   jump = None
                 });
            munch_args (i + 1, rst)))
      | [] -> []

    in
    munch_stm stm;
    List.rev !ir_list

  let canonize stm =
    Canon.linearize stm |> Canon.basicBlocks |> Canon.traceSchedule

  let debug_print_exp exp = exp |> Translate.show_exp |> Printf.eprintf "%s\n"

  let debug_print_stm stms =
    List.iter (fun stm -> stm |> Tree.show_stm |> Printf.eprintf "%s\n") stms

  let debug_print_formatted formatted =
    List.iter (Printf.eprintf "%s") formatted

  let%test_unit "test_add" =
    Translate.init ();

    let frame =
      Frame.newFrame { name = Temp.namedlabel "main"; formals = [] }
    in
    let input_string = {|
      let
        var n := 1
        var m := 2
      in
        n + m
      end
|} in
    let exp, _ = Semant.transProg (Parse.parse_string input_string) in
    let stms = canonize (Translate.unNx exp) in
    let res = List.map (fun stm -> codegen frame stm) stms |> List.flatten in
    List.iter (fun instr -> Assem.show_instr instr |> print_endline) res;
    let formatted = List.map (Assem.format Frame.string_of_register) res in
    debug_print_formatted formatted;
    ()

  let%test_unit "test_conditional_branch" =
    Translate.init ();

    let frame =
      Frame.newFrame { name = Temp.namedlabel "main"; formals = [] }
    in
    let input_string = {|
      if 1 < 2 then 100 else 200
|} in
    let exp, frag = Semant.transProg (Parse.parse_string input_string) in
    let stms = canonize (Translate.unNx exp) in
    let res = List.map (fun stm -> codegen frame stm) stms |> List.flatten in
    let formatted = List.map (Assem.format Frame.string_of_register) res in
    debug_print_formatted formatted;
    ()
end
