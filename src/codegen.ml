open Frame
module A = Assem
module T = Tree

module type CODEGEN = sig
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

module RistVGen : CODEGEN = struct
  let codegen frame stm =
    let ir_list : A.instr list ref = ref [] in
    let emit instr = ir_list := !ir_list @ [ instr ] in

    let result gen =
      let t = Temp.newtemp () in
      gen t;
      t
    in

    let rec munch_stm = function
      | T.SEQ (a, b) ->
          munch_stm a;
          munch_stm b
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)" i;
                 src = [ munch_exp e1; munch_exp e2; Temp.newtemp () ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
          emit
            (A.OPER
               {
                 assem =
                   Printf.sprintf "\taddi 's2, 's0, %d\n\tsd 's1, 0('s2)" i;
                 src = [ munch_exp e1; munch_exp e2; Temp.newtemp () ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, T.MEM e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tld 's1, 's0\n\tsd 's1, 0('s1)";
                 src = [ munch_exp e1; munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM (T.CONST i), e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tli 's0, %d\n\tsd 's1, 0('s0)" i;
                 src = [ Temp.newtemp (); munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.MEM e1, e2) ->
          emit
            (A.OPER
               {
                 assem = Printf.sprintf "\tsd 's1, 0('s0)";
                 src = [ munch_exp e1; munch_exp e2 ];
                 dst = [];
                 jump = None;
               })
      | T.MOVE (T.TEMP i, e2) ->
          emit
            (A.MOVE
               {
                 assem = Printf.sprintf "\tmove 'd0, 's0";
                 src = munch_exp e2;
                 dst = i;
               })
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
                     assem = "\tj 'j0";
                     src = [];
                     dst = [];
                     jump = Some [ label ];
                   })
          | _ -> ErrorMsg.impossible "Impossible to jump to a non-label.")
      | T.EXP exp -> ignore (munch_exp exp)
      | stm -> ErrorMsg.impossible @@ "Unmatched pattern " ^ Tree.show_stm stm
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
          in
          result (fun r ->
              emit
                (A.OPER
                   {
                     assem = Printf.sprintf "\t%s 'd0, 's0, 's1" (map_op op);
                     src = [ munch_exp e1; munch_exp e2 ];
                     dst = [ r ];
                     jump = None;
                   }))
      | T.TEMP t -> t
    in
    munch_stm stm;
    List.rev !ir_list

  let canonize stm =
    Canon.linearize stm |> Canon.basicBlocks |> Canon.traceSchedule

  let%test_unit "test_assem" =
    Translate.init ();

    let frame =
      Frame.newFrame { name = Temp.namedlabel "main"; formals = [] }
    in
    let input_string = {|
      1 + 1
|} in
    let exp, _ = Semant.transProg (Parse.parse_string input_string) in
    Translate.show_exp exp |> print_endline;
    let stms = canonize (Translate.unNx exp) in
    List.iter (fun stm -> Tree.show_stm stm |> print_endline) stms;
    let res = List.map (fun stm -> codegen frame stm) stms |> List.flatten in
    List.iter (fun instr -> Assem.show_instr instr |> print_endline) res;
    ()
end
