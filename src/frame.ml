module type FRAME = sig
  type access [@@deriving show]
  type frame [@@deriving show]
  type newFrameParams = { name : Temp.label; formals : bool list }

  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string
  [@@deriving show]

  val newFrame : newFrameParams -> frame
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val name : frame -> Temp.label

  (* frame pointer *)
  val fp : Temp.temp

  (* return value *)
  val rv : Temp.temp

  (* return address *)
  val ra : Temp.temp

  (* stack pointer *)
  val sp : Temp.temp

  val wordsize : int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

  type fn_prolog_epilog = { prolog : Assem.instr list; body : Assem.instr list; epilog : Assem.instr list }
  val procEntryExit3 : frame * Assem.instr list -> fn_prolog_epilog

  val arg_regs : Temp.temp list
  val callee_save_regs : Temp.temp list
  val caller_save_regs : Temp.temp list
  val string_of_register : Temp.temp -> string
end

module RISCVFrame : FRAME = struct
  type access = InFrame of int | InReg of Temp.temp [@@deriving show]

  type frame = { name : Temp.label; formals : access list; locals : int ref }
  [@@deriving show]

  type newFrameParams = { name : Temp.label; formals : bool list }

  type frag =
    | PROC of { body : Tree.stm; frame : frame }
    | STRING of Temp.label * string
  [@@deriving show]

  let wordsize = 8

  let formals_start_offset = 0

  let local_start_offset = 24

  let num_formals_in_registers = 8

  let buildFormalAccess formals =
    let _, _, l =
      List.fold_left
        (fun (i, pos, l) escape ->
          if i < num_formals_in_registers && not escape then
            (i + 1, pos, InReg (Temp.newtemp ()) :: l)
          else (i + 1, pos + wordsize, InFrame pos :: l))
        (0, formals_start_offset, [])
        formals
    in
    List.rev l

  let newFrame ({ name; formals } : newFrameParams) =
    { name; formals = buildFormalAccess formals; locals = ref 0 }

  let name ({ name; _ } : frame) = name
  let formals ({ formals; _ } : frame) = formals

  let allocLocal ({ locals; _ } : frame) = function
    | true ->
      incr locals;
      InFrame (local_start_offset - !locals * wordsize)
    | false -> InReg (Temp.newtemp ())

  let exp a e =
    match a with
    | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
    | InReg r -> Tree.TEMP r

  let externalCall (name, exps) =
    Tree.CALL (Tree.NAME (Temp.namedlabel name), exps)

  let procEntryExit1 (frame, stm) = stm

  module TempMap = Map.Make
      (struct
        type t = string
        let compare = String.compare
      end)

  let temp_map = ref Temp.Table.empty

  let zero = Temp.newtemp ()
  let fp = Temp.newtemp ()      (* frame pointer *)
  let s0 = Temp.newtemp ()      (* saved register *)
  let rv = Temp.newtemp ()      (* return value *)
  let ra = Temp.newtemp ()      (* return address *)
  let sp = Temp.newtemp ()      (* stack pointer *)

  let process_registers lst =
    temp_map := List.fold_left (fun m (reg_name, tmp) ->
        Temp.Table.enter m tmp reg_name) !temp_map lst;
    List.map snd lst

  let special_regs =
    process_registers [
      ("zero", zero);
      ("ra", ra);
      ("s0", s0);  (* frame pointer *)
      ("sp", sp);
      ("fp", fp);
      ("a0", rv)
    ]

  let arg_regs =
    process_registers [
      ("a1", Temp.newtemp ());
      ("a2", Temp.newtemp ());
      ("a3", Temp.newtemp ());
      ("a4", Temp.newtemp ());
      ("a5", Temp.newtemp ());
      ("a6", Temp.newtemp ());
      ("a7", Temp.newtemp ());
    ]

  let callee_save_regs =
    process_registers [
      ("s0", s0);
      ("s1", Temp.newtemp());
      ("s2", Temp.newtemp ());
      ("s3", Temp.newtemp ());
      ("s4", Temp.newtemp ());
      ("s5", Temp.newtemp ());
      ("s6", Temp.newtemp ());
      ("s7", Temp.newtemp ());
      ("s8", Temp.newtemp ());
      ("s9", Temp.newtemp ());
    ]

  let caller_save_regs =
    process_registers [
      ("t0", Temp.newtemp ());
      ("t1", Temp.newtemp ());
      ("t2", Temp.newtemp ());
      ("t3", Temp.newtemp ());
      ("t4", Temp.newtemp ());
      ("t5", Temp.newtemp ());
      ("t6", Temp.newtemp ());
    ]

  let string_of_register tmp =
    match Temp.Table.look !temp_map tmp with
    | Some reg -> reg
    | None -> Temp.makestring tmp

  let procEntryExit2 (frame, body) =
    body @ [
      Assem.OPER {
        assem = "";
        src = [ sp; fp ] @ callee_save_regs;
        dst = [];
        jump = Some []
      }
    ]

  type fn_prolog_epilog = {
    prolog : Assem.instr list;
    body : Assem.instr list;
    epilog : Assem.instr list
  }

  module A = Assem

  (* See https://inst.eecs.berkeley.edu/~cs61c/resources/RISCV_Calling_Convention.pdf *)
  let procEntryExit3 (frame, body) =
    let { name; formals; locals } = frame in
    let space = List.length formals + !locals in

    let create_prolog formals =
      let decrement =
        A.OPER {
          assem = Printf.sprintf "\taddi 's0, 's0, %d\n" (-space * wordsize);
          src = [ sp ];
          dst = [];
          jump = None }
      in
      let store_saved_regs, num_used_frame, _ =
        List.fold_left (fun (acc, i_frame, i_reg) formal ->
            match formal with
            | InFrame n ->
              acc @ [
                A.MOVE {
                  assem = Printf.sprintf "\tsd 's0, %d('d0)\n" n;
                  src = List.nth callee_save_regs (n / wordsize);
                  dst = sp
                }
              ], i_frame + 1, i_reg
            | InReg reg ->
              acc, i_frame, i_reg + 1
          ) ([], 0, 0) formals
      in
      decrement :: store_saved_regs @
      [ A.MOVE { assem = Printf.sprintf "\tsd 'd0, %d('s0)\n" (num_used_frame * wordsize);
                 src = sp;
                 dst = ra } ] (* store ra *)
    in
    let create_epilog formals =
      let back_saved_regs, num_used_frame, _ =
        List.fold_left (fun (acc, i_frame, i_reg) formal ->
            match formal with
            | InFrame n ->
              acc @ [A.MOVE { assem = Printf.sprintf "\tld 'd0, %d('s0)\n" n;
                       src = sp;
                       dst = List.nth callee_save_regs (n / wordsize) }], i_frame + 1, i_reg
            | InReg reg ->
              acc, i_frame, i_reg + 1
          ) ([], 0, 0) formals
      in
      let reload_ra =
        A.MOVE { assem = Printf.sprintf "\tld 'd0, %d('s0)\n" (num_used_frame * wordsize);
                 src = sp;
                 dst = ra }
      in
      let increment =
        A.OPER {
          assem = Printf.sprintf "\taddi 's0, 's0, %d\n" (space * wordsize);
          src = [ sp ];
          dst = [];
          jump = None }
      in
      let jump_back = A.OPER { assem = "\tjr 's0\n"; src = [ ra ]; dst = []; jump = None } in
      back_saved_regs @ [ reload_ra; increment; jump_back ]
    in
    {
      prolog = create_prolog formals;
      body = body;
      epilog = create_epilog formals;
    }

end

module Frame : FRAME = RISCVFrame
