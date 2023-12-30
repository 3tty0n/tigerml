(*
    Frames para riscv (sin registers).

0xffff
        |    ...     |  s0+16 |
        |   arg 10   |  s0+8  | 48(sp)
        |   arg 9    |  s0    | 40(sp)
        --------------  s0
        |   fp ant   |  s0-8  | 32(sp)
        | stat link  |  s0-16 | 24(sp)
        |   return   |  s0-24 | 16(sp)
        |  local 1   |  s0-32 |  8(sp)
        |  local 2   |  sp
        |    ...     |
0x0

*)

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

  type fn = { prologue : string list; body : Assem.instr list; epilogue : string list }
  val procEntryExit3 : frame * Assem.instr list -> fn

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

  let max_out_count = ref 0

  let buildFormalAccess formals =
    let formal_count, _, processed_formals =
      List.fold_left
        (fun (i, pos, l) escape ->
          if i < num_formals_in_registers && not escape then
            (i + 1, pos, InReg (Temp.newtemp ()) :: l)
          else (i + 1, pos + wordsize, InFrame pos :: l))
        (0, formals_start_offset, [])
        formals
    in
    if formal_count > !max_out_count then max_out_count := formal_count;
    List.rev processed_formals

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

  let fp_prev = -wordsize
  let fp_prev_lev = -2 * wordsize
  let ra_prev = -3 * wordsize

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

  let procEntryExit1 (frame, body) =

    let open Tree in
    let { name; formals; locals } = frame in
    let set_param (stmt, n) access =
      if n < 4 then
        match access with
        | InReg t ->
          SEQ (
            MOVE (TEMP t, TEMP (List.nth arg_regs n)),
            stmt
          ), n + 1
        | InFrame k ->
          SEQ (
            MOVE (MEM (BINOP (PLUS, TEMP fp, CONST k)),
                  TEMP (List.nth arg_regs n)),
            stmt
          ), n + 1
      else
        match access with
        | InReg t ->
          SEQ (
            MOVE (TEMP t,
                  MEM (BINOP (PLUS, TEMP fp, CONST (n * wordsize)))),
            stmt
          ), n + 1
        | InFrame k ->
          stmt, n + 1
    in
    let body, _ = List.fold_left set_param (body, 0) formals in

    let save_load_reg stmt temp =
      let open Tree in
      let access = allocLocal frame false in
      let location_exp = exp access (TEMP fp) in
      SEQ (
        SEQ (
          MOVE (location_exp, TEMP (temp)),
          stmt
        ),
        MOVE (TEMP temp, location_exp)
      )
    in
    (* List.fold_left save_load_reg body callee_save_regs *)
    (* |> fun stmt -> List.fold_left save_load_reg stmt [ra] *)
    body

  let procEntryExit2 (frame, body) =
    body @ [
      Assem.OPER {
        assem = "";
        src = [ ra; sp; fp ] @ callee_save_regs;
        dst = [];
        jump = Some []
      }
    ]

  type fn = {
    prologue : string list;
    body : Assem.instr list;
    epilogue : string list
  }

  module A = Assem

  let procEntryExit3 (frame, body) =

    let { name; formals; locals } = frame in

    let space = List.length formals + !locals in (* number of s registers used *)
    let prologue =
      Printf.sprintf "%s:\n" (Symbol.name name) ::
      Printf.sprintf "\taddi, sp, sp, %d\n" (-space * wordsize) ::
      Printf.sprintf "\tsd, ra, %d(sp)\n" ((space - 1) * wordsize) ::
      Printf.sprintf "\tsd, s0, %d(sp)\n" ((space - 2) * wordsize) ::
      Printf.sprintf "\taddi, s0, sp, %d\n" (space * wordsize) :: []
    in
    let epilogue =
      Printf.sprintf "\tld, ra, %d(sp)\n" ((space - 1) * wordsize) ::
      Printf.sprintf "\tld, s0, %d(sp)\n" ((space - 2) * wordsize) ::
      Printf.sprintf "\taddi, s0, sp, %d\n" (space * wordsize) ::
      Printf.sprintf "\tjr ra\n" :: []
    in
    { prologue = prologue; body = body; epilogue = epilogue }

end

module Frame : FRAME = RISCVFrame
