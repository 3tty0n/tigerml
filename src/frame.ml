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

  val arg_regs : Temp.temp list
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

  let buildFormalAccess formals =
    let compine (n_frame, lst) = function
      | true -> (n_frame + 1, InFrame ((n_frame + 1) * 4) :: lst)
      | false -> (n_frame, InReg (Temp.newtemp ()) :: lst)
    in

    let _, revrsedAccess = List.fold_left compine (0, []) formals in
    List.rev revrsedAccess

  let newFrame ({ name; formals } : newFrameParams) =
    { name; formals = buildFormalAccess formals; locals = ref 0 }

  let name ({ name; _ } : frame) = name
  let formals ({ formals; _ } : frame) = formals

  let allocLocal ({ locals; _ } : frame) = function
    | true ->
        incr locals;
        InFrame (!locals * 4)
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

  let fp = Temp.newtemp ()      (* frame pointer *)
  let rv = Temp.newtemp ()      (* return value *)
  let ra = Temp.newtemp ()      (* return address *)
  let sp = Temp.newtemp ()      (* stack pointer *)
  let wordsize = 8

  let process_registers lst =
    temp_map := List.fold_left (fun m (reg_name, tmp) ->
        Temp.Table.enter m tmp reg_name) !temp_map lst;
    List.map snd lst

  let special_regs =
    process_registers [
      ("zero", Temp.newtemp ());
      ("ra", ra);
      ("s0", Temp.newtemp ());  (* frame pointer *)
      ("sp", sp);
      ("fp", fp);
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

  let calle_regs =
    process_registers [
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

end

module Frame : FRAME = RISCVFrame
