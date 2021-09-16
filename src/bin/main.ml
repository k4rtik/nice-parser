open Example.Ast
open Example.Lexer
open Example.Parser

let rec interpret (e : expr) =
  match e with
  | ELiteral i ->
      i
  | EBinOp (e1, OpPlus, e2) ->
      interpret e1 + interpret e2
  | EBinOp (e1, OpMinus, e2) ->
      interpret e1 - interpret e2
  | EBinOp (e1, OpTimes, e2) ->
      interpret e1 * interpret e2
  | EBinOp (e1, OpDiv, e2) ->
      interpret e1 / interpret e2
  | EUnOp (OpNeg, e) ->
      - (interpret e)

let process (line : string) =
  Example.Parser.parse_string line
  |> sexp_of_expr
  |> Base.Sexp.to_string_hum
  |> Printf.printf "-> %s\n";
  Printf.printf "%d\n%!" (interpret (parse_string line))

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  (* enable pretty error messages *)
  Example.Parser.pp_exceptions ();
  repeat (Lexing.from_channel stdin)
