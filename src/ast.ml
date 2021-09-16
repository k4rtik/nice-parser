open Base

type unop =
  | OpNeg
  [@@deriving sexp]

type binop =
  | OpPlus | OpMinus | OpTimes | OpDiv
  [@@deriving sexp]

(* type 'a located =
  { loc: Lexing.position * Lexing.position; value: 'a } *)

type expr =
| ELiteral of int
| EUnOp of unop * expr
| EBinOp of expr * binop * expr
[@@deriving sexp]
