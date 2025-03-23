open Base

type t = unit Lwt.t

let none : t = Lwt.return_unit
let batch : t list -> t = Lwt_list.iter_p Fn.id
