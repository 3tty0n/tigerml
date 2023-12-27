
module Hashtbl = MoreLabels.Hashtbl

type symbol = { name : string; symbol : int }
[@@deriving show]

type t = symbol
[@@deriving show]

let symbols = Hashtbl.create 16

let next =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let unique name =
  { name; symbol = next () }

let symbol name =
  match Hashtbl.find_opt symbols name with
  | Some symbol -> { name; symbol }
  | None ->
    let t = unique name in
    Hashtbl.replace symbols ~key:t.name ~data:t.symbol;
    t

let name { name; _ } = name

module Table = Map.Make(
  struct
    type t = symbol
    let compare {name=_; symbol=n1} {name=_; symbol=n2} = compare n1 n2
  end)

type 'a table = 'a Table.t

let empty = Table.empty

let enter (t : 'a table) (k : symbol) (v : 'a) = Table.add k v t

let look (t : 'a table) (k : symbol) = Table.find_opt k t
