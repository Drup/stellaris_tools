open Ast

type scope =
  | All of Id.t
  | Any of Id.t

type _ expr =
  | GEq : Id.t * num value -> bool expr
  | LEq : Id.t * num value -> bool expr
  | Eq  : Id.t * 'a value -> bool expr
  | And : bool expr list -> bool expr
  | Or  : bool expr list -> bool expr
  | Not : bool expr list -> bool expr
  | Val : 'a value -> 'a expr
  | If : bool expr * 'a expr -> 'a expr
  | Hidden : 'a expr list -> 'a expr
  | Scope : scope * 'a expr list -> 'a expr

type _ ty =
  | Bool : bool ty
  | Int : int ty
  | Float : float ty
  | Id : Id.t ty

type aty = A : _ ty -> aty

exception Expected of (Id.t * Raw.value Loc.t * aty)
exception ExpectedV of (Loc.loc * Raw.value * aty)
exception ExpectedClause of (Id.t * aty * aty)

let expected_ty ~ty id ty' =
  raise (ExpectedClause (id, A ty, A ty'))

let rec typecheck
  : type t . t ty -> Raw.expr -> t expr
  = fun ty { id; value} -> match id.v, ty with
    | "NOT", Bool -> Not (typecheck_list Bool value)
    | "AND", Bool -> And (typecheck_list Bool value)
    | "OR", Bool  -> Or (typecheck_list Bool value)
    | ("NOT" | "AND" | "OR"), _ -> expected_ty ~ty id Bool

    | "hidden", ty -> Hidden (typecheck_list ty value)
    | _ -> raise (Expected (id, value, A ty))

and typecheck_list
  : type t . t ty -> Raw.value Loc.t -> t expr list
  = fun ty { v ; loc } -> match v with
    | List v -> List.map (typecheck ty) v
    | _ -> raise (ExpectedV (loc, v, A ty))

(* and typecheck_value *)
(*   : type t . t ty -> Raw.value -> t expr *)
(*   = fun ty v -> match ty, v with *)
(*     | Bool, Bool b -> Bool b *)
(*     | Int, Int i -> Int i *)
