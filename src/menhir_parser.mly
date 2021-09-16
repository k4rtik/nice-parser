(* tokens *)
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

// %left PLUS MINUS        /* lowest precedence */
// %left TIMES DIV         /* medium precedence */
// %nonassoc UMINUS        /* highest precedence */

(* start symbol *)
%start <Ast.expr> main
%{ open Ast %}

%%

let main :=
  ~ = expr; EOL; <>

let expr ==
  additive_expr

let additive_expr :=
  | multiplicative_expr
  // | located(
 |     ~ = additive_expr; ~ = additive_op; ~ = multiplicative_expr; <EBinOp>
    // )

let additive_op ==
  | PLUS;  { OpPlus }
  | MINUS; { OpMinus }

let multiplicative_expr :=
  | atomic_expr
  // | located(
|      ~ = multiplicative_expr; ~ = multiplicative_op; ~ = atomic_expr; <EBinOp>
    // )

let multiplicative_op ==
  | TIMES; { OpTimes }
  | DIV;   { OpDiv }

let atomic_expr :=
  | LPAREN; ~ = expr; RPAREN; <>
  // | located(
    | ~ = INT; <ELiteral>
    | ~ = unary_op; ~ = atomic_expr; <EUnOp>
    // )

let unary_op ==
  | MINUS; { OpNeg }
