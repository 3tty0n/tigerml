module type FRAME = sig
  type access
  [@@deriving show]

  type frame
  [@@deriving show]

  type newFrameParams = { name: Temp.label; formals : bool list }

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
  val wordsize : int
  val exp : access -> Tree.exp -> Tree.exp

  val externalCall : string * Tree.exp list -> Tree.exp

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
end

module RISCVFrame : FRAME = struct
  type access = InFrame of int
              | InReg of Temp.temp
  [@@deriving show]

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

  let newFrame ({name; formals} : newFrameParams) =
    { name=name; formals=buildFormalAccess formals; locals=ref 0 }

  let name ({name; _} : frame) = name

  let formals ({formals; _} : frame) = formals

  let allocLocal ({locals; _} : frame) = function
    | true ->
      incr locals;
      InFrame (!locals * 4)
    | false -> InReg (Temp.newtemp ())

  let fp = Temp.newtemp ()
  let rv = Temp.newtemp ()

  let wordsize = 8

  let exp a e =
    match a with
    | InFrame k -> Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST k))
    | InReg r -> Tree.TEMP r

  let externalCall (name, exps) =
    Tree.CALL (Tree.NAME (Temp.namedlabel name), exps)

  let procEntryExit1 (frame, stm) = stm

end

module Frame : FRAME = RISCVFrame
