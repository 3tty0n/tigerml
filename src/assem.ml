open Core

type reg = string
type temp = Temp.temp [@@deriving show]
type label = Temp.label [@@deriving show]

type instr =
  | OPER of {
      assem : string;
      dst : temp list;
      src : temp list;
      jump : label list option;
    }
  | LABEL of { assem : string; lab : label }
  | MOVE of { assem : string; dst : temp; src : temp }
[@@deriving show]

let format : (temp -> string) -> instr -> string =
 fun f instr ->
  match instr with
  | OPER { assem; dst; src; jump } ->
      let jump' = match jump with Some l -> l | None -> [] in
      let res =
        List.fold2_exn ~init:assem
          ~f:(fun s i label ->
            Str.global_replace (Str.regexp ("'s" ^ Int.to_string i)) (f label) s)
          (List.range 0 (List.length src))
          src
      in
      let res2 =
        List.fold2_exn ~init:res
          ~f:(fun s i label ->
            Str.global_replace (Str.regexp ("'d" ^ Int.to_string i)) (f label) s)
          (List.range 0 (List.length dst))
          dst
      in
      List.fold2_exn ~init:res2
        ~f:(fun s i label ->
          Str.global_replace
            (Str.regexp ("'j" ^ Int.to_string i))
            (Temp.string_of_label label)
            s)
        (List.range 0 (List.length jump'))
        jump'
  | LABEL { assem } -> assem
  | MOVE { assem; dst; src } ->
      let res = Str.global_replace (Str.regexp "'s0") (f src) assem in
      Str.global_replace (Str.regexp "'d0") (f dst) res
