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

type kind = Effect | Trigger
let string_of_kind = function
  | Effect -> "effect"
  | Trigger -> "trigger"
let pp_kind = Fmt.of_to_string string_of_kind

module SSet = CCSet.Make(String)

type t = {
  name : string ;
  kind : kind ;
  desc : string ;
  scopes : SSet.t ;
  targets : SSet.t ;
}
let pp ppf { name ; kind ; desc ; scopes ; targets } =
  Format.fprintf ppf
    "@[<v2>%a %s:@,%a@,@[<2>Scopes: %a@]@,@[<2>Targets: %a@]@]@."
    pp_kind kind name
    Fmt.(box text) desc
    Fmt.(iter ~sep:sp SSet.iter string) scopes
    Fmt.(iter ~sep:sp SSet.iter string) targets

let pp_raw ppf { name ; kind = _ ; desc ; scopes ; targets } =
  Format.fprintf ppf
    "%s - %s@.Supported Scopes: %a@.@.Supported targets: %a@.@."
    name desc
    Fmt.(iter ~sep:(const char ' ') SSet.iter string) scopes
    Fmt.(iter ~sep:(const char ' ') SSet.iter string) targets

module Wiki = struct

  let pp_one ppf { name ; kind = _ ; desc ; scopes ; targets } =
    Format.fprintf ppf
      "|-@.\
       | %s@.\
       | %s@.\
       | %a@.\
       | %a@."
      name desc
      Fmt.(iter ~sep:(const char ' ') SSet.iter string) scopes
      Fmt.(iter ~sep:(const char ' ') SSet.iter string) targets

  let pp_header ppf =
    Format.fprintf ppf
      "|-@.\
       ! Name !! Desc !! Scopes !! Target@."

  let prelude =
    {wiki|{| class="wikitable sortable" style="font-size:90%; text-align:left" width="100%"|wiki}

  let pp ppf l =
    Fmt.pf ppf
      "%s@. %t%a@.|}@."
      prelude
      pp_header Fmt.(list ~sep:nop pp_one) l
end


let regex =
  let open Re in
  let first_line = seq [
      bol ;
      group (rep1 @@ compl [char '-']) ;
      str " - " ;
      group (rep1 notnl) ;
      rep1 (char '\n') ;
    ]
  and scopes = seq [
      str "Supported Scopes:" ;
      group (rep notnl) ;
      rep1 (char '\n') ;
    ]
  and targets = seq [
      str "Supported targets:" ;
      group (rep notnl) ;
      rep1 (char '\n') ;
    ]
  in
  compile @@ seq [ first_line ; scopes ; targets ]

let blank_re = Re.(compile blank)

let extract kind g =
  let process_list l =
    SSet.of_list @@ List.map String.lowercase_ascii @@
    Re.split blank_re l
  in
  let name = String.trim @@ Re.Group.get g 1 in
  let desc = String.trim @@ Re.Group.get g 2 in
  let scopes = process_list @@ Re.Group.get g 3 in
  let targets = process_list @@ Re.Group.get g 4 in
  { name ; kind ; desc ; scopes ; targets }

let parse kind s =
  List.map (extract kind) @@ Re.all regex s

let main () =
  let file = Sys.argv.(1) in
  let filecontent = CCIO.read_all @@ open_in file in
  let content = parse Trigger filecontent  in
  Format.printf "%a@."
    (* (Fmt.(list ~sep:nop) pp_raw) *)
    Wiki.pp
    content

let () = main ()