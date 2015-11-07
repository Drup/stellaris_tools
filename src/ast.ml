module Loc = struct

  type loc = {
    col : int ;
    line : int ;
    file : string ;
  }

  type 'a t = { v : 'a ; loc : loc }

  let make ~loc v = { v ; loc }

  let empty = { col = 0 ; line = 0 ; file = "" }

end

module Id = struct

  type t = string Loc.t

  type long =
    | Base of string
    | Dot of string * long

end

module Raw = struct

  type expr = {
    id : Id.t ;
    value : value Loc.t ;
  }

  and value =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Lid of Id.long
    | List of expr list

end

type _ value =
  | Int : int -> int value
  | Float : float -> float value
  | Bool : bool -> bool value
  | Id : Id.t -> Id.t value
