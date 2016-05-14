(*
 * Copyright (c) 2016 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
