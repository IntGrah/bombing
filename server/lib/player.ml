open Base

type t = A | B | C | D [@@deriving show, eq, yojson_of]
type team = AC | BD [@@deriving show, yojson_of]

let next : t -> t = function A -> B | B -> C | C -> D | D -> A
let teammate : t -> t = Fn.compose next next
let team : t -> team = function A | C -> AC | B | D -> BD

type 'a store = Store of 'a * 'a * 'a * 'a [@@deriving show, yojson_of]

let get (p : t) (Store (a, b, c, d) : 'a store) : 'a =
  match p with A -> a | B -> b | C -> c | D -> d

let init (x : 'a) : 'a store = Store (x, x, x, x)

let set (player : t) (x : 'a) (Store (a, b, c, d) : 'a store) : 'a store =
  match player with
  | A -> Store (x, b, c, d)
  | B -> Store (a, x, c, d)
  | C -> Store (a, b, x, d)
  | D -> Store (a, b, c, x)

type position = Big_master | Small_master | Small_slave | Big_slave
[@@deriving show, eq, yojson_of]

let mapi ~(f : t -> 'a -> 'b) (Store (a, b, c, d) : 'a store) : 'b store =
  Store (f A a, f B b, f C c, f D d)

let to_list (Store (a, b, c, d) : 'a store) : 'a list = [ a; b; c; d ]

let who_is (pos : position) (Store (a, b, c, d) : position store) : t =
  if equal_position pos a then A
  else if equal_position pos b then B
  else if equal_position pos c then C
  else if equal_position pos d then D
  else failwith "Not found"
