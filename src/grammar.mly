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

%{
open Ast
%}

%token EOF
%token <string> ID
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token DOT
%token EQUAL
%token LACCO RACCO
%token SHARP

%start main_expr
%type <Ast.Raw.expr> main_expr

%%

main_expr: d=expr EOF {d}

expr:
  id=id EQUAL value=value
  { {Raw. id; value = Loc.make ~loc:Loc.empty value } }

value:
  | v = STRING { Raw.String v }
  | v = INT { Raw.Int v }
  | v = FLOAT { Raw.Float v }
  | v = lid { Raw.Lid v }
  | LACCO l = expr* RACCO { Raw.List l }

id: id=ID { Loc.make ~loc:Loc.empty id }
lid:
  | id=ID DOT lid=lid { Id.Dot (id, lid) }
  | id=ID { Id.Base id }