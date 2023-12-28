type exp = unit

type access = InFrame of int
            | InReg of Temp.temp

type frame = { name : Temp.label; formals : access list; locals : int ref }

type newFrameParams = { name : Temp.label; formals : bool list }

let exp = ()

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
