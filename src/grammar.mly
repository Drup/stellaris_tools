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